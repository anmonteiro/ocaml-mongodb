open Ppxlib
open Ast_helper

module Ast_builder_default_loc = struct
  include Ppx_deriving.Ast_convenience

  let gen_def_loc f x =
    let loc = !Ast_helper.default_loc in
    f ~loc x

  let lid = gen_def_loc Ast_builder.Default.Located.lident

  let list = gen_def_loc Ast_builder.Default.elist

  let pstr = gen_def_loc Ast_builder.Default.pstring

  let plist = gen_def_loc Ast_builder.Default.plist

  let lam = gen_def_loc Ast_builder.Default.pexp_fun Nolabel None
end

open Ast_builder_default_loc

let disable_warning_39 () =
  let loc = !Ast_helper.default_loc in
  let name = { txt = "ocaml.warning"; loc } in
  Ast_helper.Attr.mk ~loc name (PStr [%str "-39"])

let mod_mknoloc x = mknoloc (Some x)

let deriver = "bson"

let raise_errorf = Ppx_deriving.raise_errorf

let argn = Printf.sprintf "arg%d"

let attr_int_encoding attrs =
  match
    Ppx_deriving.attr ~deriver "encoding" attrs
    |> Ppx_deriving.Arg.(
         get_attr ~deriver (enum [ "string"; "int32"; "int64" ]))
  with
  | Some "string" ->
    `String
  | Some "int32" ->
    `Int32
  | Some "int64" | None ->
    `Int64
  | _ ->
    assert false

let attr_string name default attrs =
  match
    Ppx_deriving.attr ~deriver name attrs
    |> Ppx_deriving.Arg.(get_attr ~deriver string)
  with
  | Some x ->
    x
  | None ->
    default

let attr_key = attr_string "key"

let attr_name = attr_string "name"

let attr_ser attrs =
  Ppx_deriving.(
    attrs |> attr ~deriver "to_bson" |> Arg.(get_attr ~deriver expr))

let attr_desu attrs =
  Ppx_deriving.(
    attrs |> attr ~deriver "of_bson" |> Arg.(get_attr ~deriver expr))

let attr_default attrs =
  Ppx_deriving.attr ~deriver "default" attrs
  |> Ppx_deriving.Arg.(get_attr ~deriver expr)

type options =
  { is_strict : bool
  ; want_meta : bool
  ; want_exn : bool
  }

let parse_options options =
  let strict = ref true in
  let meta = ref false in
  let exn = ref false in
  let get_bool = Ppx_deriving.Arg.(get_expr ~deriver bool) in
  options
  |> List.iter (fun (name, expr) ->
         match name with
         | "strict" ->
           strict := get_bool expr
         | "meta" ->
           meta := get_bool expr
         | "exn" ->
           exn := get_bool expr
         | _ ->
           raise_errorf
             ~loc:expr.pexp_loc
             "%s does not support option %s"
             deriver
             name);
  { is_strict = !strict; want_meta = !meta; want_exn = !exn }

let poly_fun names expr =
  List.fold_right
    (fun name expr ->
      let loc = name.Location.loc in
      let name = name.Location.txt in
      [%expr fun [%p pvar ("poly_" ^ name)] -> [%e expr]])
    names
    expr

let type_add_attrs typ attributes =
  { typ with ptyp_attributes = typ.ptyp_attributes @ attributes }

let rec ser_expr_of_typ typ =
  match attr_ser typ.ptyp_attributes with
  | Some e ->
    e
  | None ->
    ser_expr_of_only_typ typ

and ser_expr_of_only_typ typ =
  let loc = typ.ptyp_loc in
  let attr_int_encoding typ =
    match attr_int_encoding typ with
    | `String ->
      "String"
    | `Int32 ->
      "Int32"
    | `Int64 ->
      "Int64"
  in
  match typ with
  | [%type: unit] ->
    [%expr fun (x : Ppx_deriving_runtime.unit) -> `Null]
  | [%type: int] ->
    [%expr fun (x : Ppx_deriving_runtime.int) -> `Int64 (Int64.of_int x)]
  | [%type: float] ->
    [%expr fun (x : Ppx_deriving_runtime.float) -> `Double x]
  | [%type: bool] ->
    [%expr fun (x : Ppx_deriving_runtime.bool) -> `Boolean x]
  | [%type: string] ->
    [%expr fun (x : Ppx_deriving_runtime.string) -> `String x]
  | [%type: bytes] ->
    [%expr fun x -> `Binary (Generic (Bytes.to_string x))]
  | [%type: char] ->
    [%expr fun x -> `String (String.make 1 x)]
  | [%type: [%t? typ] ref] ->
    [%expr fun x -> [%e ser_expr_of_typ typ] !x]
  | [%type: [%t? typ] list] ->
    [%expr fun x -> `Array (safe_map [%e ser_expr_of_typ typ] x)]
  | [%type: int32] | [%type: Int32.t] ->
    [%expr fun x -> `Int32 x]
  | [%type: int64] | [%type: Int64.t] ->
    [%expr
      fun x ->
        [%e
          Exp.variant (attr_int_encoding typ.ptyp_attributes) (Some [%expr x])]]
  | [%type: nativeint] | [%type: Nativeint.t] ->
    [%expr
      fun x ->
        [%e
          Exp.variant
            (attr_int_encoding typ.ptyp_attributes)
            (Some [%expr Nativeint.to_string x])]]
  | [%type: [%t? typ] array] ->
    [%expr
      fun x -> `Array (Array.to_list (Array.map [%e ser_expr_of_typ typ] x))]
  | [%type: [%t? typ] option] ->
    [%expr function None -> `Null | Some x -> [%e ser_expr_of_typ typ] x]
  | [%type: Bson.t] ->
    [%expr fun x -> x]
  | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, args); _ } ->
    let fwd =
      app
        (Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Suffix "to_bson") lid)))
        (List.map ser_expr_of_typ args)
    in
    (* eta-expansion is necessary for let-rec *)
    [%expr fun x -> [%e fwd] x]
  | { ptyp_desc = Ptyp_tuple typs; _ } ->
    [%expr
      fun [%p ptuple (List.mapi (fun i _ -> pvar (argn i)) typs)] ->
        `Array
          [%e
            list
              (List.mapi
                 (fun i typ -> app (ser_expr_of_typ typ) [ evar (argn i) ])
                 typs)]]
  | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc; _ } ->
    let cases =
      fields
      |> List.map (fun (field : row_field) ->
             match field.prf_desc with
             | Rtag (label, true (*empty*), []) ->
               let label = label.txt in
               let attrs = field.prf_attributes in
               Exp.case
                 (Pat.variant label None)
                 [%expr `Array [ `String [%e str (attr_name label attrs)] ]]
             | Rtag (label, false, [ { ptyp_desc = Ptyp_tuple typs; _ } ]) ->
               let label = label.txt in
               let attrs = field.prf_attributes in
               Exp.case
                 (Pat.variant
                    label
                    (Some (ptuple (List.mapi (fun i _ -> pvar (argn i)) typs))))
                 [%expr
                   `Array
                     (`String [%e str (attr_name label attrs)]
                     :: [%e
                          list
                            (List.mapi
                               (fun i typ ->
                                 app (ser_expr_of_typ typ) [ evar (argn i) ])
                               typs)])]
             | Rtag (label, false, [ typ ]) ->
               let label = label.txt in
               let attrs = field.prf_attributes in
               Exp.case
                 (Pat.variant label (Some [%pat? x]))
                 [%expr
                   `Array
                     [ `String [%e str (attr_name label attrs)]
                     ; [%e ser_expr_of_typ typ] x
                     ]]
             | Rinherit ({ ptyp_desc = Ptyp_constr (tname, _); _ } as typ) ->
               Exp.case
                 [%pat? [%p Pat.type_ tname] as x]
                 [%expr [%e ser_expr_of_typ typ] x]
             | _ ->
               raise_errorf
                 ~loc:ptyp_loc
                 "%s cannot be derived for %s"
                 deriver
                 (Ppx_deriving.string_of_core_type typ))
    in
    Exp.function_ cases
  | { ptyp_desc = Ptyp_var name; _ } ->
    [%expr ([%e evar ("poly_" ^ name)] : _ -> Bson.t)]
  | { ptyp_desc = Ptyp_alias (typ, name); _ } ->
    [%expr
      fun x ->
        [%e evar ("poly_" ^ name)] x;
        [%e ser_expr_of_typ typ] x]
  | { ptyp_desc = Ptyp_poly (names, typ); _ } ->
    poly_fun names (ser_expr_of_typ typ)
  | { ptyp_loc; _ } ->
    raise_errorf
      ~loc:ptyp_loc
      "%s cannot be derived for %s"
      deriver
      (Ppx_deriving.string_of_core_type typ)

(* http://desuchan.net/desu/src/1284751839295.jpg *)
let rec desu_fold ~loc ~path f typs =
  typs
  |> List.mapi (fun i typ ->
         i, app (desu_expr_of_typ ~path typ) [ evar (argn i) ])
  |> List.fold_left
       (fun x (i, y) ->
         let loc = x.pexp_loc in
         [%expr [%e y] >>= fun [%p pvar (argn i)] -> [%e x]])
       [%expr Result.Ok [%e f (List.mapi (fun i _ -> evar (argn i)) typs)]]

and desu_expr_of_typ ~path typ =
  match attr_desu typ.ptyp_attributes with
  | Some e ->
    e
  | None ->
    desu_expr_of_only_typ ~path typ

and desu_expr_of_only_typ ~path typ =
  let loc = typ.ptyp_loc in
  let error = [%expr Result.Error [%e str (String.concat "." path)]] in
  let decode' cases =
    Exp.function_
      (List.map (fun (pat, exp) -> Exp.case pat exp) cases
      @ [ Exp.case [%pat? _] error ])
  in
  let decode pat exp = decode' [ pat, exp ] in
  match typ with
  | [%type: unit] ->
    decode [%pat? `Null] [%expr Result.Ok ()]
  | [%type: int] ->
    decode [%pat? `Int64 x] [%expr Result.Ok x]
  | [%type: float] ->
    decode'
      [ [%pat? `Int32 x], [%expr Result.Ok (Int32.to_float x)]
      ; [%pat? `Int64 x], [%expr Result.Ok (Int64.to_float x)]
      ; [%pat? `Double x], [%expr Result.Ok x]
      ]
  | [%type: bool] ->
    decode [%pat? `Bool x] [%expr Result.Ok x]
  | [%type: string] ->
    decode [%pat? `String x] [%expr Result.Ok x]
  | [%type: bytes] ->
    decode [%pat? `Binary (Generic x)] [%expr Result.Ok (Bytes.of_string x)]
  | [%type: char] ->
    decode
      [%pat? `String x]
      [%expr if String.length x = 1 then Result.Ok x.[0] else [%e error]]
  | [%type: int32] | [%type: Int32.t] ->
    decode' [ [%pat? `Int32 x], [%expr Result.Ok x] ]
  | [%type: int64] | [%type: Int64.t] ->
    (match attr_int_encoding typ.ptyp_attributes with
    | `String ->
      decode [%pat? `String x] [%expr Result.Ok (Int64.of_string x)]
    | _ ->
      decode'
        [ [%pat? `Int64 x], [%expr Result.Ok x]
        ; [%pat? `Int32 x], [%expr Result.Ok (Int64.of_int32 x)]
        ])
  | [%type: nativeint] | [%type: Nativeint.t] ->
    (match attr_int_encoding typ.ptyp_attributes with
    | `String ->
      decode [%pat? `String x] [%expr Result.Ok (Nativeint.of_string x)]
    | _ ->
      decode'
        [ [%pat? `Int64 x], [%expr Result.Ok (Int64.to_nativeint x)]
        ; [%pat? `Int32 x], [%expr Result.Ok (Nativeint.of_int32 x)]
        ])
  | [%type: [%t? typ] ref] ->
    [%expr
      fun x -> [%e desu_expr_of_typ ~path:(path @ [ "contents" ]) typ] x >|= ref]
  | [%type: [%t? typ] option] ->
    [%expr
      function
      | `Null ->
        Result.Ok None
      | x ->
        [%e desu_expr_of_typ ~path typ] x >>= fun x -> Result.Ok (Some x)]
  | [%type: [%t? typ] list] ->
    decode
      [%pat? `Array xs]
      [%expr map_bind [%e desu_expr_of_typ ~path typ] [] xs]
  | [%type: [%t? typ] array] ->
    decode
      [%pat? `Array xs]
      [%expr map_bind [%e desu_expr_of_typ ~path typ] [] xs >|= Array.of_list]
  | [%type: Bson.t] ->
    [%expr fun x -> Result.Ok x]
  | { ptyp_desc = Ptyp_tuple typs; _ } ->
    decode
      [%pat? `Array [%p plist (List.mapi (fun i _ -> pvar (argn i)) typs)]]
      (desu_fold ~loc ~path tuple typs)
  | { ptyp_desc = Ptyp_variant (fields, _, _); ptyp_loc; _ } ->
    let inherits, tags =
      List.partition
        (fun field ->
          match field.prf_desc with Rinherit _ -> true | _ -> false)
        fields
    in
    let tag_cases =
      tags
      |> List.map (fun field ->
             match field.prf_desc with
             | Rtag (label, true (*empty*), []) ->
               let label = label.txt in
               let attrs = field.prf_attributes in
               Exp.case
                 [%pat? `Array [ `String [%p pstr (attr_name label attrs)] ]]
                 [%expr Result.Ok [%e Exp.variant label None]]
             | Rtag (label, false, [ { ptyp_desc = Ptyp_tuple typs; _ } ]) ->
               let label = label.txt in
               let attrs = field.prf_attributes in
               Exp.case
                 [%pat?
                   `Array
                     (`String [%p pstr (attr_name label attrs)]
                     :: [%p plist (List.mapi (fun i _ -> pvar (argn i)) typs)])]
                 (desu_fold
                    ~loc
                    ~path
                    (fun x -> Exp.variant label (Some (tuple x)))
                    typs)
             | Rtag (label, false, [ typ ]) ->
               let label = label.txt in
               let attrs = field.prf_attributes in
               Exp.case
                 [%pat? `Array [ `String [%p pstr (attr_name label attrs)]; x ]]
                 [%expr
                   [%e desu_expr_of_typ ~path typ] x >>= fun x ->
                   Result.Ok [%e Exp.variant label (Some [%expr x])]]
             | Rinherit ({ ptyp_desc = Ptyp_constr (tname, _); _ } as typ) ->
               Exp.case
                 [%pat? [%p Pat.type_ tname] as x]
                 [%expr [%e desu_expr_of_typ ~path typ] x]
             | _ ->
               raise_errorf
                 ~loc:ptyp_loc
                 "%s cannot be derived for %s"
                 deriver
                 (Ppx_deriving.string_of_core_type typ))
    and inherits_case =
      let toplevel_typ = typ in
      inherits
      |> List.map (fun field ->
             match field.prf_desc with Rinherit typ -> typ | _ -> assert false)
      |> List.fold_left
           (fun expr typ ->
             [%expr
               match [%e desu_expr_of_typ ~path typ] json with
               | Result.Ok result ->
                 Result.Ok (result :> [%t toplevel_typ])
               | Result.Error _ ->
                 [%e expr]])
           error
      |> Exp.case [%pat? _]
    in
    [%expr
      fun (json : Bson.t) ->
        [%e Exp.match_ [%expr json] (tag_cases @ [ inherits_case ])]]
  | { ptyp_desc = Ptyp_constr ({ txt = lid; _ }, args); _ } ->
    let fwd =
      app
        (Exp.ident (mknoloc (Ppx_deriving.mangle_lid (`Suffix "of_bson") lid)))
        (List.map (desu_expr_of_typ ~path) args)
    in
    (* eta-expansion is necessary for recursive groups *)
    [%expr fun x -> [%e fwd] x]
  | { ptyp_desc = Ptyp_var name; _ } ->
    [%expr ([%e evar ("poly_" ^ name)] : Bson.t -> _ error_or)]
  | { ptyp_desc = Ptyp_alias (typ, name); _ } ->
    [%expr
      fun x ->
        [%e evar ("poly_" ^ name)] x;
        [%e desu_expr_of_typ ~path typ] x]
  | { ptyp_desc = Ptyp_poly (names, typ); _ } ->
    poly_fun names (desu_expr_of_typ ~path typ)
  | { ptyp_loc; _ } ->
    raise_errorf
      ~loc:ptyp_loc
      "%s cannot be derived for %s"
      deriver
      (Ppx_deriving.string_of_core_type typ)

(* TODO: Do not wrap runtime around [@default ...]. We do currently and for
   instance the following doesn't currently work: module List = struct let x =
   [1; 2] end type t = {field : int list [@default List.x]} [@@deriving
   to_yojson] *)
let wrap_runtime decls =
  Ppx_deriving.sanitize ~module_:(Lident "Ppx_deriving_bson_runtime") decls

let ser_type_of_decl ~options ~path:_ type_decl =
  ignore (parse_options options);
  let loc = type_decl.ptype_loc in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let polymorphize =
    Ppx_deriving.poly_arrow_of_type_decl
      (fun var -> [%type: [%t var] -> Bson.t])
      type_decl
  in
  polymorphize [%type: [%t typ] -> Bson.t]

let ser_str_of_record ~loc varname labels =
  let fields =
    labels
    |> List.mapi
         (fun
           _i
           { pld_loc = loc
           ; pld_name = { txt = name; _ }
           ; pld_type
           ; pld_attributes
           ; _
           }
         ->
           let field = Exp.field (evar varname) (mknoloc (Lident name)) in
           let result =
             [%expr
               [%e str (attr_key name pld_attributes)]
               , [%e ser_expr_of_typ @@ type_add_attrs pld_type pld_attributes]
                   [%e field]]
           in
           match attr_default (pld_type.ptyp_attributes @ pld_attributes) with
           | None ->
             [%expr [%e result] :: fields]
           | Some default ->
             [%expr
               if [%e field] = [%e default] then
                 fields
               else
                 [%e result] :: fields])
  in
  let assoc =
    List.fold_left
      (fun expr field ->
        let loc = expr.pexp_loc in
        [%expr
          let fields = [%e field] in
          [%e expr]])
      [%expr `Document fields]
      fields
  in
  [%expr
    let fields = [] in
    [%e assoc]]

let ser_str_of_type ~options ~path ({ ptype_loc = loc; _ } as type_decl) =
  ignore (parse_options options);
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  match type_decl.ptype_kind with
  | Ptype_open ->
    let to_bson_name =
      Ppx_deriving.mangle_type_decl (`Suffix "to_bson") type_decl
    in
    let mod_name =
      Ppx_deriving.mangle_type_decl (`PrefixSuffix ("M", "to_bson")) type_decl
    in
    (match type_decl.ptype_manifest with
    | Some
        ({ ptyp_desc = Ptyp_constr ({ txt = lid; _ }, _args); _ } as manifest)
      ->
      let ser = ser_expr_of_typ manifest in
      let lid = Ppx_deriving.mangle_lid (`PrefixSuffix ("M", "to_bson")) lid in
      let orig_mod = Mod.ident (mknoloc lid) in
      ( [ Str.module_ (Mb.mk (mod_mknoloc mod_name) orig_mod) ]
      , [ Vb.mk
            (pvar to_bson_name)
            (polymorphize [%expr ([%e ser] : [%t typ] -> Bson.t)])
        ]
      , [] )
    | Some _ ->
      raise_errorf
        ~loc
        "%s: extensible type manifest should be a type name"
        deriver
    | None ->
      let poly_vars =
        List.rev
          (Ppx_deriving.fold_left_type_decl
             (fun acc name -> name :: acc)
             []
             type_decl)
      in
      let polymorphize_ser =
        Ppx_deriving.poly_arrow_of_type_decl
          (fun var -> [%type: [%t var] -> Bson.Safe.t])
          type_decl
      in
      let ty =
        Typ.poly poly_vars (polymorphize_ser [%type: [%t typ] -> Bson.Safe.t])
      in
      let default_fun =
        let type_path =
          String.concat "." (path @ [ type_decl.ptype_name.txt ])
        in
        let e_type_path =
          Ast_builder.Default.estring ~loc:Location.none type_path
        in
        [%expr
          fun _ ->
            invalid_arg
              ("to_bson: Maybe a [@@deriving bson] is missing when extending \
                the type "
              ^ [%e e_type_path])]
      in
      let poly_fun = polymorphize default_fun in
      let poly_fun =
        Ppx_deriving.fold_left_type_decl
          (fun exp name -> Exp.newtype name exp)
          poly_fun
          type_decl
      in
      let mod_name = "M_" ^ to_bson_name in
      let typ =
        Type.mk
          ~kind:(Ptype_record [ Type.field ~mut:Mutable (mknoloc "f") ty ])
          (mknoloc "t_to_bson")
      in
      let record = Vb.mk (pvar "f") (Exp.record [ lid "f", poly_fun ] None) in
      let flid = lid (Printf.sprintf "%s.f" mod_name) in
      let field = Exp.field (Exp.ident flid) flid in
      let mod_ =
        Str.module_
          (Mb.mk
             (mod_mknoloc mod_name)
             (Mod.structure
                [ Str.type_ Nonrecursive [ typ ]
                ; Str.value Nonrecursive [ record ]
                ]))
      in
      [ mod_ ], [ Vb.mk (pvar to_bson_name) [%expr fun x -> [%e field] x] ], [])
  | kind ->
    let serializer =
      match kind, type_decl.ptype_manifest with
      | Ptype_open, _ ->
        assert false
      | Ptype_abstract, Some manifest ->
        ser_expr_of_typ manifest
      | Ptype_variant constrs, _ ->
        constrs
        |> List.map
             (fun { pcd_name = { txt = name'; _ }; pcd_args; pcd_attributes; _ }
             ->
               let json_name = attr_name name' pcd_attributes in
               match pcd_args with
               | Pcstr_tuple [] ->
                 Exp.case
                   (pconstr name' [])
                   [%expr `Array [ `String [%e str json_name] ]]
               | Pcstr_tuple args ->
                 let arg_exprs =
                   List.mapi
                     (fun i typ -> app (ser_expr_of_typ typ) [ evar (argn i) ])
                     args
                 in
                 Exp.case
                   (pconstr name' (List.mapi (fun i _ -> pvar (argn i)) args))
                   [%expr
                     `Array (`String [%e str json_name] :: [%e list arg_exprs])]
               | Pcstr_record labels ->
                 let arg_expr = ser_str_of_record ~loc (argn 0) labels in
                 Exp.case
                   (pconstr name' [ pvar (argn 0) ])
                   [%expr
                     `Array
                       (`String [%e str json_name] :: [%e list [ arg_expr ]])])
        |> Exp.function_
      | Ptype_record labels, _ ->
        [%expr fun x -> [%e ser_str_of_record ~loc "x" labels]]
      | Ptype_abstract, None ->
        raise_errorf
          ~loc
          "%s cannot be derived for fully abstract types"
          deriver
    in
    let ty = ser_type_of_decl ~options ~path type_decl in
    let fv = Ppx_deriving.free_vars_in_core_type ty in
    let poly_type = Typ.force_poly @@ Typ.poly fv @@ ty in
    let var_s = Ppx_deriving.mangle_type_decl (`Suffix "to_bson") type_decl in
    let var = pvar var_s in
    ( []
    , [ Vb.mk
          ~attrs:[ disable_warning_39 () ]
          (Pat.constraint_ var poly_type)
          (polymorphize [%expr [%e wrap_runtime serializer]])
      ]
    , [ Str.value
          Nonrecursive
          [ Vb.mk [%expr [%e pvar "_"]] [%expr [%e evar var_s]] ]
      ] )

let ser_str_of_type_ext
    ~options ~path:_ ({ ptyext_path = { loc; _ }; _ } as type_ext)
  =
  ignore (parse_options options);
  let serializer =
    let pats =
      List.fold_right
        (fun { pext_name = { txt = name'; _ }; pext_kind; pext_attributes; _ }
             acc_cases ->
          match pext_kind with
          | Pext_rebind _ ->
            (* nothing to do, since the constructor must be handled in original
               constructor declaration *)
            acc_cases
          | Pext_decl (_, pext_args, _) ->
            let json_name = attr_name name' pext_attributes in
            let case =
              match pext_args with
              | Pcstr_tuple [] ->
                Exp.case
                  (pconstr name' [])
                  [%expr `Array [ `String [%e str json_name] ]]
              | Pcstr_tuple args ->
                let arg_exprs =
                  List.mapi
                    (fun i typ -> app (ser_expr_of_typ typ) [ evar (argn i) ])
                    args
                in
                Exp.case
                  (pconstr name' (List.mapi (fun i _ -> pvar (argn i)) args))
                  [%expr
                    `Array (`String [%e str json_name] :: [%e list arg_exprs])]
              | Pcstr_record _ ->
                raise_errorf
                  ~loc
                  "%s: record variants are not supported in extensible types"
                  deriver
            in
            case :: acc_cases)
        type_ext.ptyext_constructors
        []
    in
    let fallback_case =
      Exp.case
        [%pat? x]
        [%expr
          [%e Ppx_deriving.poly_apply_of_type_ext type_ext [%expr fallback]] x]
    in
    Exp.function_ (pats @ [ fallback_case ])
  in
  let mod_name =
    let mod_lid =
      Ppx_deriving.mangle_lid
        (`PrefixSuffix ("M", "to_bson"))
        type_ext.ptyext_path.txt
    in
    Longident.name mod_lid
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_ext type_ext in
  let serializer = polymorphize (wrap_runtime serializer) in
  let flid = lid (Printf.sprintf "%s.f" mod_name) in
  let set_field = Exp.setfield (Exp.ident flid) flid serializer in
  let field = Exp.field (Exp.ident flid) flid in
  let body =
    [%expr
      let fallback = [%e field] in
      [%e set_field]]
  in
  [ Str.value
      ?loc:None
      Nonrecursive
      [ Vb.mk (Pat.construct (lid "()") None) body ]
  ]

let error_or typ =
  let loc = typ.ptyp_loc in
  [%type: [%t typ] Ppx_deriving_bson_runtime.error_or]

let desu_type_of_decl_poly ~options ~path:_ type_decl type_ =
  ignore (parse_options options);
  let loc = type_decl.ptype_loc in
  let polymorphize =
    Ppx_deriving.poly_arrow_of_type_decl
      (fun var -> [%type: Bson.t -> [%t error_or var]])
      type_decl
  in
  polymorphize type_

let desu_type_of_decl ~options ~path type_decl =
  let loc = type_decl.ptype_loc in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  desu_type_of_decl_poly
    ~options
    ~path
    type_decl
    [%type: Bson.t -> [%t error_or typ]]

let desu_str_of_record ~loc ~is_strict ~error ~path wrap_record labels =
  let top_error = error path in
  let record =
    List.fold_left
      (fun expr i ->
        let loc = expr.pexp_loc in
        [%expr [%e evar (argn i)] >>= fun [%p pvar (argn i)] -> [%e expr]])
      (let r =
         Exp.record
           (labels
           |> List.mapi (fun i { pld_name = { txt = name; _ }; _ } ->
                  mknoloc (Lident name), evar (argn i)))
           None
       in
       [%expr Result.Ok [%e wrap_record r]])
      (labels |> List.mapi (fun i _ -> i))
  in
  let default_case = if is_strict then top_error else [%expr loop xs _state] in
  let cases =
    (labels
    |> List.mapi
         (fun
           i
           { pld_loc = loc
           ; pld_name = { txt = name; _ }
           ; pld_type
           ; pld_attributes
           ; _
           }
         ->
           let path = path @ [ name ] in
           let thunks =
             labels
             |> List.mapi (fun j _ ->
                    if i = j then
                      app
                        (desu_expr_of_typ ~path
                        @@ type_add_attrs pld_type pld_attributes)
                        [ evar "x" ]
                    else
                      evar (argn j))
           in
           Exp.case
             [%pat? ([%p pstr (attr_key name pld_attributes)], x) :: xs]
             [%expr loop xs [%e tuple thunks]]))
    @ [ Exp.case [%pat? []] record; Exp.case [%pat? _ :: xs] default_case ]
  and thunks =
    labels
    |> List.map
         (fun { pld_name = { txt = name; _ }; pld_type; pld_attributes; _ } ->
           match attr_default (pld_type.ptyp_attributes @ pld_attributes) with
           | None ->
             error (path @ [ name ])
           | Some x ->
             [%expr Result.Ok [%e x]])
  in
  [%expr
    function
    | `Document xs ->
      let rec loop
          xs
          ([%p ptuple (List.mapi (fun i _ -> pvar (argn i)) labels)] as _state)
        =
        [%e Exp.match_ [%expr xs] cases]
      in
      loop xs [%e tuple thunks]
    | _ ->
      [%e top_error]]

let desu_str_of_type ~options ~path ({ ptype_loc = loc; _ } as type_decl) =
  let { is_strict; want_exn; _ } = parse_options options in
  let path = path @ [ type_decl.ptype_name.txt ] in
  let error path = [%expr Result.Error [%e str (String.concat "." path)]] in
  let top_error = error path in
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  match type_decl.ptype_kind with
  | Ptype_open ->
    let of_bson_name =
      Ppx_deriving.mangle_type_decl (`Suffix "of_bson") type_decl
    in
    let mod_name =
      Ppx_deriving.mangle_type_decl (`PrefixSuffix ("M", "of_bson")) type_decl
    in
    (match type_decl.ptype_manifest with
    | Some
        ({ ptyp_desc = Ptyp_constr ({ txt = lid; _ }, _args); _ } as manifest)
      ->
      let desu = desu_expr_of_typ ~path manifest in
      let lid = Ppx_deriving.mangle_lid (`PrefixSuffix ("M", "of_bson")) lid in
      let orig_mod = Mod.ident (mknoloc lid) in
      let poly_desu =
        polymorphize [%expr ([%e wrap_runtime desu] : Bson.t -> _)]
      in
      ( [ Str.module_ (Mb.mk (mod_mknoloc mod_name) orig_mod) ]
      , [ Vb.mk (pvar of_bson_name) poly_desu ]
      , [] )
    | Some _ ->
      raise_errorf
        ~loc
        "%s: extensible type manifest should be a type name"
        deriver
    | None ->
      let poly_vars =
        List.rev
          (Ppx_deriving.fold_left_type_decl
             (fun acc name -> name :: acc)
             []
             type_decl)
      in
      let polymorphize_desu =
        Ppx_deriving.poly_arrow_of_type_decl
          (fun var -> [%type: Bson.t -> [%t error_or var]])
          type_decl
      in
      let ty =
        Typ.poly
          poly_vars
          (polymorphize_desu [%type: Bson.t -> [%t error_or typ]])
      in
      let default_fun = Exp.function_ [ Exp.case [%pat? _] top_error ] in
      let poly_fun = polymorphize default_fun in
      let poly_fun =
        Ppx_deriving.fold_left_type_decl
          (fun exp name -> Exp.newtype name exp)
          poly_fun
          type_decl
      in
      let mod_name = "M_" ^ of_bson_name in
      let typ =
        Type.mk
          ~kind:(Ptype_record [ Type.field ~mut:Mutable (mknoloc "f") ty ])
          (mknoloc "t_of_bson")
      in
      let record = Vb.mk (pvar "f") (Exp.record [ lid "f", poly_fun ] None) in
      let flid = lid (Printf.sprintf "%s.f" mod_name) in
      let field = Exp.field (Exp.ident flid) flid in
      let mod_ =
        Str.module_
          (Mb.mk
             (mod_mknoloc mod_name)
             (Mod.structure
                [ Str.type_ Nonrecursive [ typ ]
                ; Str.value Nonrecursive [ record ]
                ]))
      in
      [ mod_ ], [ Vb.mk (pvar of_bson_name) [%expr fun x -> [%e field] x] ], [])
  | kind ->
    let desurializer =
      match kind, type_decl.ptype_manifest with
      | Ptype_open, _ ->
        assert false
      | Ptype_abstract, Some manifest ->
        desu_expr_of_typ ~path manifest
      | Ptype_variant constrs, _ ->
        let cases =
          List.map
            (fun { pcd_loc = loc
                 ; pcd_name = { txt = name'; _ }
                 ; pcd_args
                 ; pcd_attributes
                 ; _
                 } ->
              match pcd_args with
              | Pcstr_tuple args ->
                Exp.case
                  [%pat?
                    `Array
                      (`String [%p pstr (attr_name name' pcd_attributes)]
                      :: [%p plist (List.mapi (fun i _ -> pvar (argn i)) args)]
                      )]
                  (desu_fold ~loc ~path (fun x -> constr name' x) args)
              | Pcstr_record labels ->
                let wrap_record r = constr name' [ r ] in
                let sub =
                  desu_str_of_record
                    ~loc
                    ~is_strict
                    ~error
                    ~path
                    wrap_record
                    labels
                in
                Exp.case
                  [%pat?
                    `Array
                      (`String [%p pstr (attr_name name' pcd_attributes)]
                      :: [%p plist [ pvar (argn 0) ]])]
                  [%expr [%e sub] [%e evar (argn 0)]])
            constrs
        in
        Exp.function_ (cases @ [ Exp.case [%pat? _] top_error ])
      | Ptype_record labels, _ ->
        desu_str_of_record ~loc ~is_strict ~error ~path (fun r -> r) labels
      | Ptype_abstract, None ->
        raise_errorf
          ~loc
          "%s cannot be derived for fully abstract types"
          deriver
    in
    let ty = desu_type_of_decl ~options ~path type_decl in
    let fv = Ppx_deriving.free_vars_in_core_type ty in
    let poly_type = Typ.force_poly @@ Typ.poly fv @@ ty in
    let var_s = Ppx_deriving.mangle_type_decl (`Suffix "of_bson") type_decl in
    let var = pvar var_s in
    let var_s_exn = var_s ^ "_exn" in
    let { ptype_params; _ } = type_decl in
    let var_s_exn_args = List.mapi (fun i _ -> argn i |> evar) ptype_params in
    let var_s_exn_args = var_s_exn_args @ [ evar "x" ] in
    let var_s_exn_fun =
      let rec loop = function
        | [] ->
          wrap_runtime
            [%expr
              match [%e app (evar var_s) var_s_exn_args] with
              | Result.Ok x ->
                x
              | Result.Error err ->
                raise (Failure err)]
        | hd :: tl ->
          lam (pvar hd) (loop tl)
      in
      loop (List.mapi (fun i _ -> argn i) ptype_params @ [ "x" ])
    in
    ( []
    , [ Vb.mk
          ~attrs:[ disable_warning_39 () ]
          (Pat.constraint_ var poly_type)
          (polymorphize [%expr [%e wrap_runtime desurializer]])
      ]
    , [ Str.value
          Nonrecursive
          [ Vb.mk [%expr [%e pvar "_"]] [%expr [%e evar var_s]] ]
      ]
      @
      if not want_exn then
        []
      else
        [ Str.value
            Nonrecursive
            [ Vb.mk [%expr [%e pvar var_s_exn]] var_s_exn_fun ]
        ; Str.value
            Nonrecursive
            [ Vb.mk [%expr [%e pvar "_"]] [%expr [%e evar var_s_exn]] ]
        ] )

let desu_str_of_type_ext
    ~options ~path ({ ptyext_path = { loc; _ }; _ } as type_ext)
  =
  ignore (parse_options options);
  let desurializer =
    let pats =
      List.fold_right
        (fun { pext_name = { txt = name'; _ }; pext_kind; pext_attributes; _ }
             acc_cases ->
          match pext_kind with
          | Pext_rebind _ ->
            (* nothing to do since it must have been handled in the original
               constructor declaration *)
            acc_cases
          | Pext_decl (_, pext_args, _) ->
            let case =
              match pext_args with
              | Pcstr_tuple args ->
                Exp.case
                  [%pat?
                    `Array
                      (`String [%p pstr (attr_name name' pext_attributes)]
                      :: [%p plist (List.mapi (fun i _ -> pvar (argn i)) args)]
                      )]
                  (desu_fold ~loc ~path (fun x -> constr name' x) args)
              | Pcstr_record _ ->
                raise_errorf
                  ~loc
                  "%s: record variants are not supported in extensible types"
                  deriver
            in
            case :: acc_cases)
        type_ext.ptyext_constructors
        []
    in
    let any_case =
      Exp.case
        (Pat.var (mknoloc "x"))
        (app
           (Ppx_deriving.poly_apply_of_type_ext type_ext [%expr fallback])
           [ [%expr x] ])
    in
    pats @ [ any_case ] |> Exp.function_
  in
  let mod_name =
    let mod_lid =
      Ppx_deriving.mangle_lid
        (`PrefixSuffix ("M", "of_bson"))
        type_ext.ptyext_path.txt
    in
    Longident.name mod_lid
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_ext type_ext in
  let desurializer = wrap_runtime (polymorphize desurializer) in
  let flid = lid (Printf.sprintf "%s.f" mod_name) in
  let set_field = Exp.setfield (Exp.ident flid) flid desurializer in
  let field = Exp.field (Exp.ident flid) flid in
  let body =
    [%expr
      let fallback = [%e field] in
      [%e set_field]]
  in
  [ Str.value
      ?loc:None
      Nonrecursive
      [ Vb.mk (Pat.construct (lid "()") None) body ]
  ]

let ser_sig_of_type ~options ~path type_decl =
  let to_bson =
    Sig.value
      (Val.mk
         (mknoloc (Ppx_deriving.mangle_type_decl (`Suffix "to_bson") type_decl))
         (ser_type_of_decl ~options ~path type_decl))
  in
  match type_decl.ptype_kind with
  | Ptype_open ->
    let mod_name =
      Ppx_deriving.mangle_type_decl (`PrefixSuffix ("M", "to_bson")) type_decl
    in
    let poly_vars =
      List.rev
        (Ppx_deriving.fold_left_type_decl
           (fun acc name -> name :: acc)
           []
           type_decl)
    in
    let typ = Ppx_deriving.core_type_of_type_decl type_decl in
    let loc = typ.ptyp_loc in
    let polymorphize_ser =
      Ppx_deriving.poly_arrow_of_type_decl
        (fun var -> [%type: [%t var] -> Bson.t])
        type_decl
    in
    let ty =
      Typ.poly poly_vars (polymorphize_ser [%type: [%t typ] -> Bson.t])
    in
    let typ =
      Type.mk
        ~kind:(Ptype_record [ Type.field ~mut:Mutable (mknoloc "f") ty ])
        (mknoloc "t_to_bson")
    in
    let record = Val.mk (mknoloc "f") (Typ.constr (lid "t_to_bson") []) in
    let mod_ =
      Sig.module_
        (Md.mk
           (mod_mknoloc mod_name)
           (Mty.signature [ Sig.type_ Nonrecursive [ typ ]; Sig.value record ]))
    in
    [ mod_; to_bson ]
  | _ ->
    [ to_bson ]

let ser_sig_of_type_ext ~options:_ ~path:_ _type_ext = []

let desu_sig_of_type ~options ~path type_decl =
  let { want_exn; _ } = parse_options options in
  let of_bson =
    Sig.value
      (Val.mk
         (mknoloc (Ppx_deriving.mangle_type_decl (`Suffix "of_bson") type_decl))
         (desu_type_of_decl ~options ~path type_decl))
  in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let loc = typ.ptyp_loc in
  let of_bson_exn =
    Sig.value
      (Val.mk
         (mknoloc
            (Ppx_deriving.mangle_type_decl (`Suffix "of_bson_exn") type_decl))
         (desu_type_of_decl_poly
            ~options
            ~path
            type_decl
            [%type: Bson.t -> [%t typ]]))
  in
  match type_decl.ptype_kind with
  | Ptype_open ->
    let mod_name =
      Ppx_deriving.mangle_type_decl (`PrefixSuffix ("M", "of_bson")) type_decl
    in
    let poly_vars =
      List.rev
        (Ppx_deriving.fold_left_type_decl
           (fun acc name -> name :: acc)
           []
           type_decl)
    in
    let typ = Ppx_deriving.core_type_of_type_decl type_decl in
    let polymorphize_desu =
      Ppx_deriving.poly_arrow_of_type_decl
        (fun var -> [%type: Bson.t -> [%t error_or var]])
        type_decl
    in
    let ty =
      Typ.poly
        poly_vars
        (polymorphize_desu [%type: Bson.t -> [%t error_or typ]])
    in
    let typ =
      Type.mk
        ~kind:(Ptype_record [ Type.field ~mut:Mutable (mknoloc "f") ty ])
        (mknoloc "t_of_bson")
    in
    let record = Val.mk (mknoloc "f") (Typ.constr (lid "t_of_bson") []) in
    let mod_ =
      Sig.module_
        (Md.mk
           (mod_mknoloc mod_name)
           (Mty.signature [ Sig.type_ Nonrecursive [ typ ]; Sig.value record ]))
    in
    [ mod_; of_bson ]
  | _ ->
    [ of_bson ] @ if not want_exn then [] else [ of_bson_exn ]

let desu_sig_of_type_ext ~options:_ ~path:_ _type_ext = []

let bson_str_fields ~options ~path:_ type_decl =
  let { want_meta; _ } = parse_options options in
  match want_meta, type_decl.ptype_kind with
  | false, _ | true, Ptype_open ->
    []
  | true, kind ->
    (match kind, type_decl.ptype_manifest with
    | Ptype_record labels, _ ->
      let loc = !Ast_helper.default_loc in
      let fields =
        labels
        |> List.map (fun { pld_name = { txt = name; _ }; pld_attributes; _ } ->
               [%expr [%e str (attr_key name pld_attributes)]])
      in
      let flist =
        List.fold_right
          (fun n acc -> [%expr [%e n] :: [%e acc]])
          fields
          [%expr []]
      in
      [ Str.module_
          (Mb.mk
             (mod_mknoloc
                (Ppx_deriving.mangle_type_decl (`Prefix "Bson_meta") type_decl))
             (Mod.structure
                [ Str.value
                    Nonrecursive
                    [ Vb.mk [%expr [%e pvar "keys"]] [%expr [%e flist]] ]
                ; Str.value
                    Nonrecursive
                    [ Vb.mk [%expr [%e pvar "_"]] [%expr [%e evar "keys"]] ]
                ]))
      ]
    | _ ->
      [])

let bson_sig_fields ~options ~path:_ type_decl =
  let { want_meta; _ } = parse_options options in
  match want_meta, type_decl.ptype_kind with
  | false, _ | true, Ptype_open ->
    []
  | true, kind ->
    (match kind, type_decl.ptype_manifest with
    | Ptype_record _, _ ->
      let loc = !Ast_helper.default_loc in
      [ Sig.module_
          (Md.mk
             (mod_mknoloc
                (Ppx_deriving.mangle_type_decl (`Prefix "Bson_meta") type_decl))
             (Mty.signature
                [ Sig.value (Val.mk (mknoloc "keys") [%type: string list]) ]))
      ]
    | _ ->
      [])

let str_of_type ~options ~path type_decl =
  let ser_pre, ser_vals, ser_post = ser_str_of_type ~options ~path type_decl in
  let desu_pre, desu_vals, desu_post =
    desu_str_of_type ~options ~path type_decl
  in
  let fields_post = bson_str_fields ~options ~path type_decl in
  ser_pre @ desu_pre, ser_vals @ desu_vals, ser_post @ desu_post @ fields_post

let str_of_type_to_bson ~options ~path type_decl =
  let ser_pre, ser_vals, ser_post = ser_str_of_type ~options ~path type_decl in
  let fields_post = bson_str_fields ~options ~path type_decl in
  ser_pre, ser_vals, ser_post @ fields_post

let str_of_type_of_bson ~options ~path type_decl =
  let desu_pre, desu_vals, desu_post =
    desu_str_of_type ~options ~path type_decl
  in
  let fields_post = bson_str_fields ~options ~path type_decl in
  desu_pre, desu_vals, desu_post @ fields_post

let str_of_type_ext ~options ~path type_ext =
  let ser_vals = ser_str_of_type_ext ~options ~path type_ext in
  let desu_vals = desu_str_of_type_ext ~options ~path type_ext in
  ser_vals @ desu_vals

let sig_of_type ~options ~path type_decl =
  ser_sig_of_type ~options ~path type_decl
  @ desu_sig_of_type ~options ~path type_decl
  @ bson_sig_fields ~options ~path type_decl

let sig_of_type_to_bson ~options ~path type_decl =
  ser_sig_of_type ~options ~path type_decl
  @ bson_sig_fields ~options ~path type_decl

let sig_of_type_of_bson ~options ~path type_decl =
  desu_sig_of_type ~options ~path type_decl
  @ bson_sig_fields ~options ~path type_decl

let sig_of_type_ext ~options ~path type_ext =
  ser_sig_of_type_ext ~options ~path type_ext
  @ desu_sig_of_type_ext ~options ~path type_ext

let structure f ~options ~path type_ =
  let pre, vals, post = f ~options ~path type_ in
  match vals with
  | [] ->
    pre @ post
  | _ ->
    pre @ [ Str.value ?loc:None Recursive vals ] @ post

let on_str_decls f ~options ~path type_decls =
  let unzip3 l =
    List.fold_right
      (fun (v1, v2, v3) (a1, a2, a3) -> v1 :: a1, v2 :: a2, v3 :: a3)
      l
      ([], [], [])
  in
  let pre, vals, post = unzip3 (List.map (f ~options ~path) type_decls) in
  List.concat pre, List.concat vals, List.concat post

let on_sig_decls f ~options ~path type_decls =
  List.concat (List.map (f ~options ~path) type_decls)

let () =
  Ppx_deriving.(
    register
      (create
         "bson"
         ~type_decl_str:(structure (on_str_decls str_of_type))
         ~type_ext_str:str_of_type_ext
         ~type_decl_sig:(on_sig_decls sig_of_type)
         ~type_ext_sig:sig_of_type_ext
         ()));
  Ppx_deriving.(
    register
      (create
         "to_bson"
         ~core_type:(fun typ ->
           let typ = Ppx_deriving.strong_type_of_type typ in
           wrap_runtime (ser_expr_of_typ typ))
         ~type_decl_str:(structure (on_str_decls str_of_type_to_bson))
         ~type_ext_str:ser_str_of_type_ext
         ~type_decl_sig:(on_sig_decls sig_of_type_to_bson)
         ~type_ext_sig:ser_sig_of_type_ext
         ()));
  Ppx_deriving.(
    register
      (create
         "of_bson"
         ~core_type:(fun typ ->
           let typ = Ppx_deriving.strong_type_of_type typ in
           wrap_runtime (desu_expr_of_typ ~path:[] typ))
         ~type_decl_str:(structure (on_str_decls str_of_type_of_bson))
         ~type_ext_str:desu_str_of_type_ext
         ~type_decl_sig:(on_sig_decls sig_of_type_of_bson)
         ~type_ext_sig:desu_sig_of_type_ext
         ()))
