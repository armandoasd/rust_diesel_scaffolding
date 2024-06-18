use convert_case::{Case, Casing};
use handlebars::{handlebars_helper, Handlebars};
use serde_json::{json, Value};
use std::collections::{BTreeMap, HashMap};
use std::fmt::format;
use std::io::{stderr, Write};
use serde::{Serialize};

pub struct ParseOutput {
    pub str_proto: String,
    pub str_request: String,
    pub str_rpc: String,
    pub str_model: String,
    pub str_ts_model: String,
    pub str_action: String,
    pub str_crud_controller: String,
    pub str_from_proto: String,
    pub str_into_proto: String,
    pub type_nd: bool,
    pub type_ndt: bool,
    pub type_nt: bool,
    pub type_bd: bool,
    pub type_ip: bool,
    pub type_uuid: bool,
    pub type_tz: bool,
    pub type_jsonb: bool,
    pub type_new_insertable: bool,
    pub type_json_serializable: bool,
    pub type_bit_bool: bool,
    pub crud_methods: Vec<String>,
}

pub fn parse(
    contents: String,
    action: &str,
    model_derives: Option<String>,
    add_table_name: bool,
    model_type_mapping: &mut HashMap<String, String>,
    diesel_version: &str,
    rust_style_fields: bool,
) -> ParseOutput {
    //Parse
    let mut str_model: String = "".to_string();
    let mut str_ts_model: String = "".to_string();
    let mut str_action: String = "".to_string();
    let mut str_crud_controller: String = "".to_string();
    let mut str_proto: String = "".to_string();
    let mut str_from_proto: String = "".to_string();
    let mut str_into_proto: String = "".to_string();
    let mut str_rpc: String = "".to_string();
    let mut str_request: String = "".to_string();
    let mut closable: bool = false;
    let (
        mut type_nd,
        mut type_ndt,
        mut type_nt,
        mut type_bd,
        mut type_ip,
        mut type_uuid,
        mut type_tz,
        mut type_jsonb,
        mut type_new_insertable,
        mut type_json_serializable,
        mut type_bit_bool,
    ) = (
        false, false, false, false, false, false, false, false, true, true, false,
    );

    let mut count: u16 = 0;
    let mut struct_name: String = "".to_string();
    let mut table_name: String = "".to_string();
    let mut crud_methods: Vec<String> = Vec::new();
    let content = contents.replace('\t', "    ");
    let lines = content.split('\n');
    let mut relationships: MappedRelationships = MappedRelationships {
        one_to_many: Vec::new(),
        many_to_one:Vec::new()
    };
    let has_one_to_many = false;
    let mut model_type_dict: HashMap<&str, &str> = [
        ("Int2", "i16"),
        ("SmallInt", "i16"), //sqlite
        ("Int4", "i32"),
        ("Integer", "i32"), //sqlite
        ("Unsigned<Integer", "u32"),
        ("Unsigned<Decimal", "f64"),
        ("Int8", "i64"),
        ("BigInt", "i64"),
        ("Numeric", "BigDecimal"),
        ("Decimal", "f64"),
        ("Text", "String"),
        ("Date", "NaiveDate"),
        ("Time", "NaiveTime"),
        ("Datetime", "NaiveDateTime"),
        ("Timestamp", "NaiveDateTime"),
        ("Timestamptz", "DateTime<Utc>"),
        ("Float4", "f32"),
        ("Float8", "f64"),
        ("Float", "f32"), //sqlite
        ("Bool", "bool"),
        ("Json", "Json"),
        ("Jsonb", "serde_json::Value"),
        ("Uuid", "Uuid"),
        ("Char", "String"),
        ("Varchar", "String"),
        ("Bytea", "Vec<u8>"),
        ("Binary", "Vec<u8>"),
        ("Varbinary", "Vec<u8>"),
        ("Blob", "Vec<u8>"),
        ("Tinyblob", "Vec<u8>"),
        ("Mediumblob", "Vec<u8>"),
        ("Longblob", "Vec<u8>"),
        ("Bit", "BitBool"),
        ("Inet", "IpNetwork"),
        ("Tinytext", "String"),
        ("Mediumtext", "String"),
        ("Longtext", "String"),
        ("Double", "f64"),
        ("Tinyint", "i8"),
        ("Unsigned<Tinyint", "u8"),
        ("Smallint", "i16"),
        ("Unsigned<Smallint", "u16"),
        ("Bigint", "i64"),
        ("Unsigned<Bigint", "u64"),
    ]
    .iter()
    .cloned()
    .collect();

    let mut ts_interface_type_dict: HashMap<&str, &str> = [
        ("Int2", "number"),
        ("SmallInt", "number"), //sqlite
        ("Int4", "number"),
        ("Integer", "number"), //sqlite
        ("Unsigned<Integer", "number"),
        ("Unsigned<Decimal", "number"),
        ("Int8", "number"),
        ("BigInt", "number"),
        ("Numeric", "number"),
        ("Decimal", "number"),
        ("Text", "string"),
        ("Date", "Date"),
        ("Time", "Date"),
        ("Datetime", "Date"),
        ("Timestamp", "Date"),
        ("Timestamptz", "Date"),
        ("Float4", "number"),
        ("Float8", "number"),
        ("Float", "number"), //sqlite
        ("Bool", "number"),
        ("Json", "Object"),
        ("Jsonb", "Object"),
        ("Uuid", "string"),
        ("Char", "string"),
        ("Varchar", "string"),
        ("Bytea", "number[]"),
        ("Binary", "number[]"),
        ("Varbinary", "number[]"),
        ("Blob", "number[]"),
        ("Tinyblob", "number[]"),
        ("Mediumblob", "number[]"),
        ("Longblob", "number[]"),
        ("Bit", "number"),
        ("Inet", "IpNetwork"),
        ("Tinytext", "string"),
        ("Mediumtext", "string"),
        ("Longtext", "string"),
        ("Double", "number"),
        ("Tinyint", "number"),
        ("Unsigned<Tinyint", "number"),
        ("Smallint", "number"),
        ("Unsigned<Smallint", "number"),
        ("Bigint", "number"),
        ("Unsigned<Bigint", "number"),
    ]
    .iter()
    .cloned()
    .collect();

    let proto_type_dict: HashMap<&str, &str> = [
        ("Int2", "int32"),
        ("Int4", "int32"),
        ("Int8", "int64"),
        ("BigInt", "int64"),
        ("Numeric", "string"),
        ("Text", "string"),
        ("Date", "NaiveDate"),
        ("Timestamp", "string"),
        ("Timestamptz", "string"),
        ("Float4", "float"),
        ("Float8", "double"),
        ("Bool", "bool"),
        ("Json", "string"),
        ("Jsonb", "string"),
        ("Varchar", "string"),
        ("Bytea", "bytes"),
        ("Inet", "string"),
        ("Uuid", "string"),
    ]
    .iter()
    .cloned()
    .collect();

    for (key, val) in model_type_mapping.iter() {
        model_type_dict.insert(key, val);
    }

    let mut is_schema = false;
    let mut pks_list: Vec<String> = vec![];
    let mut multiple_pk= false;
    // let mut is_excluding = true;
    for line in lines.clone() {
        let cmp = line.trim().to_string();
        let vec: Vec<&str> = cmp.split(' ').collect();
        let indent_depth = if is_schema { 4 } else { 0 };

        if cmp.contains("#[") || cmp.contains("joinable!(") {
            //do nothing
        } else if cmp.contains("pub mod ") && cmp.trim() != "pub mod sql_types {" {
            if is_schema {
                str_model.push_str("\n}\n\n");
            }
            str_model.push_str(&format!("pub mod {} {{\n", &vec[2]));
            is_schema = true;
        } else if cmp.contains("table!") {
            // add derives
            str_model.push_str(&format!(
                "\n{}#[derive({})]\n",
                " ".repeat(indent_depth),
                "Joinable"
            ));
            str_model.push_str(&format!(
                "{}#[derive({})]\n",
                " ".repeat(indent_depth),
                &format!(
                    "{}{{trace1}}",
                    model_derives
                        .as_deref()
                        .unwrap_or("Identifiable, Queryable, Debug, Selectable, Serialize, Deserialize, Clone, *Associations*, PartialEq")
                )
            ));
        } else if cmp.contains(") {") {
            // this line contains table name
            struct_name = propercase(vec[0]);
            table_name = vec[0].to_string();
            if is_schema {
                struct_name = if struct_name.contains('.') {
                    let _v: Vec<&str> = struct_name.split('.').collect();
                    _v[1].to_string()
                } else {
                    struct_name
                }
            }
            multiple_pk = false;
            let x: &[_] = &['(', ')', '{', '}', ',', ' '];
            pks_list = vec![];
            if vec.len() >= 3 {
                for c in &vec[1..vec.len() - 1] {
                    let pks = c.trim_matches(x);

                    pks_list.push(pks.to_string());
                }

                if pks_list.len() > 1 || pks_list[0] != "id" {
                    multiple_pk = true;
                    // str_model = str_model.replace(
                    //     "{trace1}",
                    //     if model_derives.is_none()
                    //         || (model_derives.is_some()
                    //             && !model_derives.clone().unwrap().contains("Identifiable"))
                    //     {
                    //         ", Identifiable"
                    //     } else {
                    //         ""
                    //     },
                    // );
                    if diesel_version == "2" {
                        str_model.push_str(&" ".repeat(indent_depth));
                        str_model.push_str("#[diesel(primary_key(");
                        str_model.push_str(&pks_list.join(", "));
                        str_model.push_str("))]\n");
                    } else {
                        str_model.push_str(&" ".repeat(indent_depth));
                        str_model.push_str("#[primary_key(");
                        str_model.push_str(&pks_list.join(", "));
                        str_model.push_str(")]\n");
                    }
                }
            }
            let table_name_singular = singular(&table_name.clone());
            relationships = find_relationships(lines.clone(), table_name.clone(), false);
            if multiple_pk {

            } else {
                crud_methods.push(format!("get_{table_name_singular}_by_id"));
                crud_methods.push(format!("get_{table_name}"));
                if relationships.clone().one_to_many.len() > 0 {
                    crud_methods.push(format!("get_{table_name}_eager"));
                }
                crud_methods.push(format!("add_{table_name_singular}"));
            }

            


            str_crud_controller.push_str(&generate_crud_controllers(
                table_name.clone(),
                struct_name.clone(),
                pks_list.clone(),
                multiple_pk,
                relationships.clone()
            ));

            if table_name.contains("_to_"){
                let rel_for_m_to_m = find_relationships(lines.clone(), table_name.clone(), true);
                if rel_for_m_to_m.many_to_one.len()>0 {
                    for rel in rel_for_m_to_m.many_to_one {
                        str_model.push_str(&format!(
                            "{}#[diesel(belongs_to({}))]\n",
                            " ".repeat(indent_depth),
                            rel.type_name
                        ));
                    }
                    str_model = str_model.replace("*Associations*,","Associations,");
                }
            }
            for rel in &relationships.many_to_one {
                str_model.push_str(&format!(
                    "{}#[diesel(belongs_to({}, foreign_key = {}_id))]\n",
                    " ".repeat(indent_depth),
                    rel.type_name,
                    rel.field_name
                ));
            }
            if relationships.many_to_one.len()>0 {
                str_model = str_model.replace("*Associations*,","Associations,");
            }else {
                str_model = str_model.replace("*Associations*,","");
            }

            let r_many_to_many = find_many_to_many_relationships(lines.clone(), table_name.clone());

            if r_many_to_many.len() > 0 {
                let mut joined_params = "".to_string();
                for rel in r_many_to_many {
                    joined_params.push_str(&format!(
                        "{}{} = {} by {}",
                        if joined_params.len() == 0 {
                            ""
                        }else {
                            ", "
                        },
                        rel.field_name,
                        rel.type_name,
                        propercase(&rel.table_name)
                    ));
                }
                str_model.push_str(&format!(
                    "{}#[many_to_many({})]\n",
                    " ".repeat(indent_depth),
                    joined_params
                ));
            }

            if add_table_name {
                // add #[diesel(table_name = "name")]
                str_model.push_str(&format!(
                    "{}#[diesel(table_name = crate::schema::{})]\n",
                    " ".repeat(indent_depth),
                    vec[0].split('.').last().unwrap()
                ));
            }
            if relationships.one_to_many.clone().len()>0{
                let mut joined_params = "".to_string();
                for rel in relationships.one_to_many.clone() {
                    joined_params.push_str(&format!(
                        "{}{} = {}",
                        if joined_params.len() == 0 {
                            ""
                        }else {
                            ", "
                        },
                        rel.field_name,
                        rel.type_name
                    ));
                }
                str_model.push_str(&format!(
                    "{}#[one_to_many({})]\n",
                    " ".repeat(indent_depth),
                    joined_params
                ));
            }

            str_model.push_str(&format!(
                "{}pub struct {} {{\n",
                " ".repeat(indent_depth),
                struct_name
            ));
            str_ts_model.push_str(&format!(
                "{}export interface {} {{\n",
                " ".repeat(indent_depth),
                struct_name
            ));
            str_proto.push_str(&format!("message {} {{\n", struct_name));

            str_into_proto.push_str(&format!(
                "\nimpl From<models::{}> for _name_::{} {{\n",
                struct_name, struct_name
            ));
            str_from_proto.push_str(&format!(
                "\nimpl From<_name_::{}> for models::{} {{\n",
                struct_name, struct_name
            ));

            str_into_proto.push_str(&format!(
                "    fn from(i: models::{}) -> Self {{\n",
                struct_name
            ));
            str_from_proto.push_str(&format!(
                "    fn from(i: _name_::{}) -> Self {{\n",
                struct_name
            ));

            str_from_proto.push_str(&format!("        models::{}{{\n", struct_name));
            str_into_proto.push_str(&format!(
                "        let mut o = _name_::{}::new();\n",
                struct_name
            ));
        } else if cmp.contains("->") {
            let _type = vec[2].replace(',', "");

            let dict = match action {
                "model" => &model_type_dict,
                "ts_interface" => &ts_interface_type_dict,
                _ => &proto_type_dict,
            };
            let is_optional = _type.clone().trim().starts_with("Nullable<");
            let is_nullable_array = _type.clone().contains("Array<Nullable<");
            let vec_count = _type.clone().matches("Array").count();
            let b_position = _type.find('[').unwrap_or(_type.len());
            let mut single_type = _type.clone();
            single_type.truncate(b_position);
            let warning_for_longer_lifetime: String;
            let type_string: &str = match dict.get(
                single_type
                    .replace("Array<", "")
                    .replace("Nullable<", "")
                    .replace('>', "")
                    .trim(),
            ) {
                Some(name) => name,
                None => {
                    // Show a warning and return a placeholder.
                    stderr().write_all(&format!("{} is not recognized. Please feel free to expand the HashMap. This could provide \
                    good hints: https://kotiri.com/2018/01/31/postgresql-diesel-rust-types.html\n", _type).into_bytes()).unwrap();
                    warning_for_longer_lifetime = format!("/* TODO: unknown type {} */", _type);
                    &warning_for_longer_lifetime[..]
                }
            };
            if type_string == "NaiveDate" {
                type_nd = true;
            }
            if type_string == "NaiveDateTime" {
                type_ndt = true;
            }
            if type_string == "NaiveTime" {
                type_nt = true;
            }
            if type_string == "BigDecimal" {
                type_bd = true;
            }
            if type_string == "IpNetwork" {
                type_ip = true;
            }
            if type_string == "BitBool" {
                type_bit_bool = true;
            }
            if type_string == "Uuid" {
                type_uuid = true;
            }
            if type_string == "DateTime<Utc>" {
                type_tz = true;
            }
            if type_string == "jsonb" {
                type_jsonb = true;
            }

            let type_with_wrap = if is_nullable_array {
                format!(
                    "{}{}{}",
                    "Vec<Option<".repeat(vec_count),
                    type_string,
                    ">>".repeat(vec_count)
                )
            } else {
                format!(
                    "{}{}{}",
                    "Vec<".repeat(vec_count),
                    type_string,
                    ">".repeat(vec_count)
                )
            };

            let mut field_name = vec[0].to_string();
            for rel_desc in relationships.many_to_one.iter_mut() {
                if rel_desc.field_name.eq(&field_name.replace("_id","")) {
                    let mut rel_dec_mod = rel_desc.clone();
                    let old_typename  = rel_dec_mod.clone().type_name;
                    if is_optional {
                        rel_dec_mod.type_name = format!("Option<{}>", old_typename);
                        rel_dec_mod.option = true;
                    }
                    str_model.push_str(&format!(
                        "{}#[many_to_one({})]\n",
                        " ".repeat(indent_depth + 4),
                        rel_dec_mod.type_name
                    ));
                    if is_optional {
                        rel_dec_mod.type_name = format!("Option::<model::{}>", old_typename);
                    }else {
                        rel_dec_mod.type_name = format!("model::{}", old_typename);
                    }
                    *rel_desc = rel_dec_mod;
                }
            }
            if table_name.clone().eq(&field_name) {
                str_model.push_str(&format!(
                    "{}#[diesel(column_name = \"{}\")]\n",
                    " ".repeat(indent_depth + 4),
                    field_name,
                ));
                field_name = "value".to_string();
            }
            str_model.push_str(&format!(
                "{}pub {}: {},\n",
                " ".repeat(indent_depth + 4),
                field_name,
                if is_optional {
                    format!("Option<{}>", type_with_wrap)
                } else {
                    type_with_wrap
                }
            ));
            let mut field_name = vec[0].to_string();
            str_ts_model.push_str(&format!(
                "{}{}: {},\n",
                " ".repeat(indent_depth + 4),
                if is_optional {
                    format!("{}?", field_name)
                } else {
                    field_name
                },
                if is_nullable_array {
                    format!("[{}?]", type_string)
                } else {
                    type_string.to_string()
                }
            ));
            count += 1;
            if count == 1 {
                let request_name = &format!("Enquire{}Request", &struct_name);
                str_rpc.push_str(&format!(
                    "    rpc get{} ({}) returns ({}) {{ }}\n",
                    &struct_name, &request_name, &struct_name
                ));
                str_request.push_str(&format!(
                    "message {} {{\n    int64 id =1;\n}}\n",
                    &request_name
                ));
            }

            str_proto.push_str(&format!("    {} {} = {};\n", type_string, &vec[0], count));
            str_from_proto.push_str(&format!(
                "            {}: i.get_{}(){},\n",
                &vec[0],
                &vec[0],
                match type_string {
                    "string" => ".to_string()",
                    "String" => ".to_string()",
                    "BigDecimal" => ".to_bigdecimal()",
                    _ => "",
                }
            ));
            str_into_proto.push_str(&format!(
                "        o.set_{}(i.{}{});\n",
                &vec[0],
                &vec[0],
                match type_string {
                    "string" => ".to_string()",
                    "String" => ".to_string()",
                    _ => ".into()",
                }
            ));
            //str_into_proto
            closable = true;
        } else if cmp.contains('}') && closable {
            str_action.push_str(&generate_action(
                table_name.clone(),
                struct_name.clone(),
                pks_list.clone(),
                multiple_pk,
                relationships.clone()
            ));
            count = 0;
            str_model.push_str(" ".repeat(indent_depth).as_str());
            str_model.push_str("}\n");
            str_ts_model.push_str("}\n\n");
            str_proto.push_str("}\n");
            //" ".repeat(8)
            str_from_proto.push_str("        }\n");
            str_from_proto.push_str("    }\n");
            str_into_proto.push_str("        o\n    }\n");
            str_from_proto.push_str("}\n");
            str_into_proto.push_str("}\n");
            closable = false;
        }
    }

    if is_schema {
        str_model.push_str("\n}\n");
    }
    str_model = str_model.trim().replace("{trace1}", "");
    str_model.push('\n');
    ParseOutput {
        str_proto,
        str_request,
        str_rpc,
        str_model,
        str_ts_model,
        str_action,
        str_crud_controller,
        str_from_proto,
        str_into_proto,
        type_nd,
        type_ndt,
        type_nt,
        type_bd,
        type_ip,
        type_uuid,
        type_tz,
        type_jsonb,
        type_new_insertable,
        type_json_serializable,
        type_bit_bool,
        crud_methods,
    }
}

fn propercase(s: &str) -> String {
    let mut next_cap = true;
    let mut store: Vec<char> = Vec::new();
    for c in s.chars() {
        if c == '.' {
            store.clear();
            next_cap = true;
            continue;
        }
        if c == '_' {
            next_cap = true;
            continue;
        }
        if next_cap {
            store.push(c.to_ascii_uppercase());
            next_cap = false;
        } else {
            store.push(c);
        }
    }
    if store.last() == Some(&'s') {
        store.pop();
        if store.last() == Some(&'e') {
            store.pop();
            if store.last() == Some(&'i') {
                store.pop();
                store.push('y');
            } else {
                store.push('e');
            }
        }
    }
    store.into_iter().collect()
}

fn singular(s: &String) -> String {
    return pluralizer::pluralize(s, 1, false);
    // let mut store: Vec<char> = s.chars().collect();
    // if store.last() == Some(&'s') {
    //     store.pop();
    //     if store.last() == Some(&'e') {
    //         store.pop();
    //         if store.last() == Some(&'i') {
    //             store.pop();
    //             store.push('y');
    //         } else {
    //             store.push('e');
    //         }
    //     }
    // }
    // store.into_iter().collect()
}

fn plural(s: &String) -> String {
    return pluralizer::pluralize(s, 2, false);
}

handlebars_helper!(str_contains: |source: String, needle: String| source.contains(&needle));

pub fn generate_action(
    table_name: String,
    class_name: String,
    pks_list: Vec<String>,
    multiple_pk: bool,
    relationships:MappedRelationships,
) -> std::string::String {
    let table_name_singular = singular(&table_name);
    let mut reg = Handlebars::new();
    reg.register_escape_fn(handlebars::no_escape);
    reg.register_helper("str-contains", Box::new(str_contains));
    reg.register_template_file(
        "action_template",
        "./generator/src/action_template.handlebars",
    )
    .expect("error loading template");
    let mut many_to_one_sorted = relationships.many_to_one.clone();
    many_to_one_sorted.sort_by(|a, b| a.field_name.cmp(&b.field_name));
    let has_rel = relationships.one_to_many.clone().len()>0;
    return reg.render("action_template", &json!({"table_name": table_name, "class_name":class_name, "table_name_singular": table_name_singular, "pks_list":pks_list, "multiple_pk":multiple_pk, "one_to_many":relationships.one_to_many, "many_to_one":many_to_one_sorted, "has_rel":has_rel })).expect("error rendering template");
}

pub fn generate_crud_controllers(
    table_name: String,
    class_name: String,
    pks_list: Vec<String>,
    multiple_pk: bool,
    relationships:MappedRelationships,
) -> std::string::String {
    let table_name_singular = singular(&table_name);
    let mut reg = Handlebars::new();
    reg.register_template_file(
        "action_template",
        "./generator/src/crud_controller_template.handlebars",
    )
    .expect("error loading template");
    return reg.render("action_template", &json!({"table_name": table_name, "class_name":class_name, "table_name_singular": table_name_singular, "pks_list":pks_list, "multiple_pk":multiple_pk, "one_to_many":relationships.one_to_many })).expect("error rendering template");
}

#[derive(Clone,Serialize)]
struct MappedRelationships {
    many_to_one: Vec<RelationshipDesc>,
    one_to_many: Vec<RelationshipDesc>,
}

#[derive(Clone,Serialize)]
struct RelationshipDesc {
    field_name: String,
    type_name: String,
    table_name: String,
    option: bool
}

pub fn find_relationships(lines: std::str::Split<'_, char>, table_name: String, is_many_t_m_e: bool) -> MappedRelationships {
    let mut r_many_to_one: Vec<RelationshipDesc> = Vec::new();
    let mut r_one_to_many: Vec<RelationshipDesc> = Vec::new();
    let pattern_one_to_many = format!(" -> {table_name} ");
    let pattern_many_to_one = format!("diesel::joinable!({table_name} -> ");
    let pattern_many_to_many = format!("_to_");
    for line in lines {
        if let Some(index) = line.find(&pattern_many_to_many) {
            if !(is_many_t_m_e && table_name.contains(&pattern_many_to_many)){
                continue;
            }  
        }
        if let Some(index) = line.find(&pattern_one_to_many) {
            if index > 0 {
                if let Some(before_match_str) = line.get(0..index) {
                    let related_table = before_match_str
                        .to_string()
                        .replace("diesel::joinable!(", "");
                    let type_name = propercase(&related_table);
                    let field_name = plural(&related_table);
                    let data = RelationshipDesc {
                        field_name,
                        type_name,
                        table_name:related_table,
                        option: false
                    };
                    r_one_to_many.push(data);
                }
            }
        }
        if line.contains(&pattern_many_to_one) {
            let trimmed_line = line.replace(&pattern_many_to_one, "");
            if let Some(t_name_i) = trimmed_line.find(" (") {
                if let Some(t_name) = trimmed_line.get(0..t_name_i) {
                    if let Some(end_i) = trimmed_line.find(")") {
                        if let Some(column_name) = trimmed_line.get(t_name_i + 2..end_i) {
                            let type_name = propercase(&t_name);
                            let field_name = column_name.to_string().replace("_id", "");
                            let data = RelationshipDesc {
                                field_name,
                                type_name,
                                table_name:t_name.to_string(),
                                option: false
                            };
                            r_many_to_one.push(data);
                        }
                    }
                }
            }
        }
    }

    return MappedRelationships {
        many_to_one: r_many_to_one,
        one_to_many: r_one_to_many,
    };
}

pub fn find_many_to_many_relationships(lines: std::str::Split<'_, char>, table_name: String) -> Vec<RelationshipDesc> {
    let mut r_many_to_many: Vec<RelationshipDesc> = Vec::new();
    let current_table_rel = format!(" -> {table_name} ");
    let join_line = format!("diesel::joinable!(");
    let pattern_many_to_many = format!("diesel::joinable!({table_name}_to_");
    for line in lines {
        if line.contains(&pattern_many_to_many) {
            if line.find(&current_table_rel).is_none() {
                let new_line = line.replace(&pattern_many_to_many, "");
                if let Some(f_i) = new_line.find(" ->"){
                    if let Some(f_name) = new_line.get(0..f_i){
                        let field_name = plural(&f_name.to_string());
                        if let Some(t_i) = new_line.find(" ("){
                            if let Some(table_name) = new_line.get((f_i+4)..t_i){
                                let type_name = propercase(&table_name);
                                let m_table_name = line.get(join_line.len()..pattern_many_to_many.len()+f_i).expect("can not extract table name");
                                let data = RelationshipDesc {
                                    field_name,
                                    type_name,
                                    table_name:m_table_name.to_string(),
                                    option: false
                                };
                                r_many_to_many.push(data);
                            }
                        }
                    }
                }
            }
        }
    }

    return r_many_to_many;
}
