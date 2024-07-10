use golem_wasm_ast::analysis::AnalysedType;

use crate::{Uri, Value, WitValue};

use crate::protobuf::type_annotated_value::TypeAnnotatedValue;
use crate::protobuf::typed_result::ResultValue;
use crate::protobuf::{
    r#type, PrimitiveType, Type, TypeEnum, TypeFlags, TypeList, TypeOption, TypePrimitive,
    TypeRecord, TypeResult, TypeTuple, TypedEnum, TypedFlags, TypedHandle, TypedList, TypedRecord,
    TypedTuple, TypedVariant,
};
use crate::protobuf::{NameValuePair, TypedOption};
use crate::protobuf::{TypeAnnotatedValue as RootTypeAnnotatedValue, TypedResult};
use crate::type_ext::TypeExt;

// To create TypeAnnotatedValue, we need the type-info represented in any forms,
// as far as it can be converted to `golem_wasm_rpc::protobuf::Type`
pub fn create<T: TypeExt>(value: &Value, typ: &T) -> Result<TypeAnnotatedValue, Vec<String>> {
    let tpe = typ.to_type();
    create_from_type(value, &tpe)
}

pub fn create_from_type(val: &Value, typ: &Type) -> Result<TypeAnnotatedValue, Vec<String>> {
    match val {
        Value::Bool(bool) => Ok(TypeAnnotatedValue::Bool(*bool)),
        Value::S8(value) => Ok(TypeAnnotatedValue::S8(*value as i32)),
        Value::U8(value) => Ok(TypeAnnotatedValue::U8(*value as u32)),
        Value::U32(value) => Ok(TypeAnnotatedValue::U32(*value)),
        Value::S16(value) => Ok(TypeAnnotatedValue::S16(*value as i32)),
        Value::U16(value) => Ok(TypeAnnotatedValue::U16(*value as u32)),
        Value::S32(value) => Ok(TypeAnnotatedValue::S32(*value)),
        Value::S64(value) => Ok(TypeAnnotatedValue::S64(*value)),
        Value::U64(value) => Ok(TypeAnnotatedValue::U64(*value)),
        Value::F32(value) => Ok(TypeAnnotatedValue::F32(*value)),
        Value::F64(value) => Ok(TypeAnnotatedValue::F64(*value)),
        Value::Char(value) => Ok(TypeAnnotatedValue::Char(*value as i32)),
        Value::String(value) => Ok(TypeAnnotatedValue::Str(value.clone())),

        Value::Enum(value) => match &typ.r#type {
            Some(crate::protobuf::r#type::Type::Enum(typ_enum)) => {
                match typ_enum.names.get(*value as usize) {
                    Some(name) => Ok(TypeAnnotatedValue::Enum(TypedEnum {
                        typ: typ_enum.names.clone(),
                        value: name.clone(),
                    })),
                    None => Err(vec![format!("Invalid enum value {}", value)]),
                }
            }
            _ => Err(vec![format!(
                "Unexpected type; expected an Enum type for value {}",
                value
            )]),
        },

        Value::Option(value) => match &typ.r#type {
            Some(crate::protobuf::r#type::Type::Option(typ_option)) => match value {
                Some(value) => {
                    if let Some(inner_type) = &typ_option.elem {
                        let result = create_from_type(value, inner_type)?;
                        Ok(TypeAnnotatedValue::Option(Box::new(TypedOption {
                            typ: Some((**inner_type).clone()),
                            value: Some(Box::new(RootTypeAnnotatedValue {
                                type_annotated_value: Some(result),
                            })),
                        })))
                    } else {
                        Err(vec!["Unexpected inner type for Option.".to_string()])
                    }
                }
                None => Ok(TypeAnnotatedValue::Option(Box::new(TypedOption {
                    typ: typ_option.elem.as_deref().cloned(),
                    value: None,
                }))),
            },
            _ => Err(vec!["Unexpected type; expected an Option type.".to_string()]),
        },

        Value::Tuple(values) => match &typ.r#type {
            Some(crate::protobuf::r#type::Type::Tuple(typ_tuple)) => {
                if values.len() != typ_tuple.elems.len() {
                    return Err(vec![format!(
                        "Tuple has unexpected number of elements: {} vs {}",
                        values.len(),
                        typ_tuple.elems.len(),
                    )]);
                }

                let mut errors = vec![];
                let mut results = vec![];

                for (value, tpe) in values.iter().zip(&typ_tuple.elems) {
                    match create_from_type(value, tpe) {
                        Ok(result) => results.push(result),
                        Err(errs) => errors.extend(errs),
                    }
                }

                if errors.is_empty() {
                    Ok(TypeAnnotatedValue::Tuple(TypedTuple {
                        typ: typ_tuple.elems.clone(),
                        value: results
                            .into_iter()
                            .map(|v| RootTypeAnnotatedValue {
                                type_annotated_value: Some(v),
                            })
                            .collect(),
                    }))
                } else {
                    Err(errors)
                }
            }
            _ => Err(vec!["Unexpected type; expected a Tuple type.".to_string()]),
        },

        Value::List(values) => match &typ.r#type {
            Some(crate::protobuf::r#type::Type::List(typ_list)) => {
                if let Some(inner_type) = &typ_list.elem {
                    let mut errors = vec![];
                    let mut results = vec![];

                    for value in values {
                        match create_from_type(value, inner_type) {
                            Ok(value) => results.push(value),
                            Err(errs) => errors.extend(errs),
                        }
                    }

                    if errors.is_empty() {
                        Ok(TypeAnnotatedValue::List(TypedList {
                            typ: Some((**inner_type).clone()),
                            values: results
                                .into_iter()
                                .map(|v| RootTypeAnnotatedValue {
                                    type_annotated_value: Some(v),
                                })
                                .collect(),
                        }))
                    } else {
                        Err(errors)
                    }
                } else {
                    Err(vec!["Unexpected inner type for List.".to_string()])
                }
            }
            _ => Err(vec!["Unexpected type; expected a List type.".to_string()]),
        },

        Value::Record(values) => match &typ.r#type {
            Some(crate::protobuf::r#type::Type::Record(typ_record)) => {
                if values.len() != typ_record.fields.len() {
                    return Err(vec!["The total number of field values is zero".to_string()]);
                }

                let mut errors = vec![];
                let mut results = vec![];

                for (value, field) in values.iter().zip(&typ_record.fields) {
                    if let Some(field_type) = &field.typ {
                        match create_from_type(value, field_type) {
                            Ok(res) => results.push((field.name.clone(), res)),
                            Err(errs) => errors.extend(errs),
                        }
                    } else {
                        errors.push(format!("Missing type for field {}", field.name));
                    }
                }

                if errors.is_empty() {
                    Ok(TypeAnnotatedValue::Record(TypedRecord {
                        typ: typ_record.fields.clone(),
                        value: results
                            .into_iter()
                            .map(|(name, value)| NameValuePair {
                                name,
                                value: Some(RootTypeAnnotatedValue {
                                    type_annotated_value: Some(value),
                                }),
                            })
                            .collect(),
                    }))
                } else {
                    Err(errors)
                }
            }
            _ => Err(vec!["Unexpected type; expected a Record type.".to_string()]),
        },

        Value::Variant {
            case_idx,
            case_value,
        } => match &typ.r#type {
            Some(crate::protobuf::r#type::Type::Variant(typ_variant)) => {
                if (*case_idx as usize) < typ_variant.cases.len() {
                    let cases = typ_variant.cases.clone();

                    let (case_name, case_tpe) = match cases.get(*case_idx as usize) {
                        Some(tpe) => Ok((tpe.name.clone(), tpe.typ.clone())),
                        None => Err(vec!["Variant not found in the expected types.".to_string()]),
                    }?;

                    match case_tpe {
                        Some(tpe) => match case_value {
                            Some(case_value) => {
                                let result = create_from_type(case_value, &tpe)?;

                                Ok(TypeAnnotatedValue::Variant(Box::new(TypedVariant {
                                    typ: Some(crate::protobuf::TypeVariant { cases }),
                                    case_name: case_name.clone(),
                                    case_value: Some(Box::new(RootTypeAnnotatedValue {
                                        type_annotated_value: Some(result),
                                    })),
                                })))
                            }
                            None => Err(vec![format!("Missing value for case {case_name}")]),
                        },
                        None => Ok(TypeAnnotatedValue::Variant(Box::new(TypedVariant {
                            typ: Some(crate::protobuf::TypeVariant { cases }),
                            case_name: case_name.clone(),
                            case_value: None,
                        }))),
                    }
                } else {
                    Err(vec![
                        "Invalid discriminant value for the variant.".to_string()
                    ])
                }
            }
            _ => Err(vec!["Unexpected type; expected a Variant type.".to_string()]),
        },

        Value::Flags(values) => match &typ.r#type {
            Some(crate::protobuf::r#type::Type::Flags(typ_flags)) => {
                if values.len() != typ_flags.names.len() {
                    return Err(vec![format!(
                        "Unexpected number of flag states: {:?} vs {:?}",
                        values.len(),
                        typ_flags.names.len()
                    )]);
                }

                let enabled_flags: Vec<String> = values
                    .iter()
                    .zip(typ_flags.names.iter())
                    .filter_map(|(enabled, name)| if *enabled { Some(name.clone()) } else { None })
                    .collect();

                Ok(TypeAnnotatedValue::Flags(TypedFlags {
                    typ: typ_flags.names.clone(),
                    values: enabled_flags,
                }))
            }
            _ => Err(vec!["Unexpected type; expected a Flags type.".to_string()]),
        },

        Value::Result(value) => match &typ.r#type {
            Some(crate::protobuf::r#type::Type::Result(typ_result)) => {
                match (value, &typ_result.ok, &typ_result.err) {
                    (Ok(Some(value)), Some(ok_type), _) => {
                        let result = create_from_type(value, ok_type)?;

                        Ok(TypeAnnotatedValue::Result(Box::new(TypedResult {
                            ok: Some(ok_type.as_ref().clone()),
                            error: typ_result.err.clone().map(|t| (*t).clone()),
                            result_value: Some(ResultValue::OkValue(Box::new(
                                RootTypeAnnotatedValue {
                                    type_annotated_value: Some(result),
                                },
                            ))),
                        })))
                    }
                    (Ok(None), Some(_), _) => {
                        Err(vec!["Non-unit ok result has no value".to_string()])
                    }

                    (Ok(None), None, _) => Ok(TypeAnnotatedValue::Result(Box::new(TypedResult {
                        ok: typ_result.ok.clone().map(|t| (*t).clone()),
                        error: typ_result.err.clone().map(|t| (*t).clone()),
                        result_value: Some(ResultValue::OkValue(Box::new(
                            RootTypeAnnotatedValue {
                                type_annotated_value: None,
                            },
                        ))),
                    }))),

                    (Ok(Some(_)), None, _) => Err(vec!["Unit ok result has a value".to_string()]),

                    (Err(Some(value)), _, Some(err_type)) => {
                        let result = create_from_type(value, err_type)?;

                        Ok(TypeAnnotatedValue::Result(Box::new(TypedResult {
                            ok: typ_result.ok.clone().map(|t| (*t).clone()),
                            error: typ_result.err.clone().map(|t| (*t).clone()),
                            result_value: Some(ResultValue::ErrorValue(Box::new(
                                RootTypeAnnotatedValue {
                                    type_annotated_value: Some(result),
                                },
                            ))),
                        })))
                    }

                    (Err(None), _, Some(_)) => {
                        Err(vec!["Non-unit error result has no value".to_string()])
                    }

                    (Err(None), _, None) => Ok(TypeAnnotatedValue::Result(Box::new(TypedResult {
                        ok: typ_result.ok.clone().map(|t| (*t).clone()),
                        error: typ_result.err.clone().map(|t| (*t).clone()),
                        result_value: Some(ResultValue::ErrorValue(Box::new(
                            RootTypeAnnotatedValue {
                                type_annotated_value: None,
                            },
                        ))),
                    }))),

                    (Err(Some(_)), _, None) => {
                        Err(vec!["Unit error result has a value".to_string()])
                    }
                }
            }

            _ => Err(vec!["Unexpected type; expected a Result type.".to_string()]),
        },

        Value::Handle { uri, resource_id } => match &typ.r#type {
            Some(crate::protobuf::r#type::Type::Handle(typ_handle)) => {
                let handle = TypedHandle {
                    uri: uri.value.clone(),
                    resource_id: *resource_id,
                    typ: Some(typ_handle.clone()),
                };
                Ok(TypeAnnotatedValue::Handle(handle))
            }
            _ => Err(vec![
                "Unexpected type; expected a Resource type.".to_string()
            ]),
        },
    }
}

pub fn get_analysed_type(value: &TypeAnnotatedValue) -> Result<AnalysedType, String> {
    let raw_type = get_type(value)?;
    AnalysedType::from_type(&raw_type)
}

pub fn get_type(value: &TypeAnnotatedValue) -> Result<Type, String> {
    let r#type = match value {
        TypeAnnotatedValue::Bool(_) => Ok(r#type::Type::Primitive(TypePrimitive {
            primitive: PrimitiveType::Bool as i32,
        })),
        TypeAnnotatedValue::S8(_) => Ok(r#type::Type::Primitive(TypePrimitive {
            primitive: PrimitiveType::S8 as i32,
        })),
        TypeAnnotatedValue::U8(_) => Ok(r#type::Type::Primitive(TypePrimitive {
            primitive: PrimitiveType::U8 as i32,
        })),
        TypeAnnotatedValue::S16(_) => Ok(r#type::Type::Primitive(TypePrimitive {
            primitive: PrimitiveType::S16 as i32,
        })),
        TypeAnnotatedValue::U16(_) => Ok(r#type::Type::Primitive(TypePrimitive {
            primitive: PrimitiveType::U16 as i32,
        })),
        TypeAnnotatedValue::S32(_) => Ok(r#type::Type::Primitive(TypePrimitive {
            primitive: PrimitiveType::S32 as i32,
        })),
        TypeAnnotatedValue::U32(_) => Ok(r#type::Type::Primitive(TypePrimitive {
            primitive: PrimitiveType::U32 as i32,
        })),
        TypeAnnotatedValue::S64(_) => Ok(r#type::Type::Primitive(TypePrimitive {
            primitive: PrimitiveType::S64 as i32,
        })),
        TypeAnnotatedValue::U64(_) => Ok(r#type::Type::Primitive(TypePrimitive {
            primitive: PrimitiveType::U64 as i32,
        })),
        TypeAnnotatedValue::F32(_) => Ok(r#type::Type::Primitive(TypePrimitive {
            primitive: PrimitiveType::F32 as i32,
        })),
        TypeAnnotatedValue::F64(_) => Ok(r#type::Type::Primitive(TypePrimitive {
            primitive: PrimitiveType::F64 as i32,
        })),
        TypeAnnotatedValue::Char(_) => Ok(r#type::Type::Primitive(TypePrimitive {
            primitive: PrimitiveType::Chr as i32,
        })),
        TypeAnnotatedValue::Str(_) => Ok(r#type::Type::Primitive(TypePrimitive {
            primitive: PrimitiveType::Str as i32,
        })),
        TypeAnnotatedValue::List(TypedList { typ, values: _ }) => {
            if let Some(typ) = typ.clone() {
                Ok(r#type::Type::List(Box::new(TypeList {
                    elem: Some(Box::new(typ)),
                })))
            } else {
                Err("Missing type for List".to_string())
            }
        }
        TypeAnnotatedValue::Tuple(TypedTuple { typ, value: _ }) => {
            Ok(r#type::Type::Tuple(TypeTuple { elems: typ.clone() }))
        }
        TypeAnnotatedValue::Record(TypedRecord { typ, value: _ }) => {
            Ok(r#type::Type::Record(TypeRecord {
                fields: typ.clone(),
            }))
        }
        TypeAnnotatedValue::Flags(TypedFlags { typ, values: _ }) => {
            Ok(r#type::Type::Flags(TypeFlags { names: typ.clone() }))
        }
        TypeAnnotatedValue::Enum(TypedEnum { typ, value: _ }) => {
            Ok(r#type::Type::Enum(TypeEnum { names: typ.clone() }))
        }
        TypeAnnotatedValue::Option(option) => {
            let typ = option.typ.clone();
            Ok(r#type::Type::Option(Box::new(TypeOption {
                elem: typ.map(Box::new),
            })))
        }
        TypeAnnotatedValue::Result(result0) => Ok(r#type::Type::Result(Box::new(TypeResult {
            ok: result0.ok.clone().map(Box::new),
            err: result0.error.clone().map(Box::new),
        }))),
        TypeAnnotatedValue::Handle(TypedHandle { typ, .. }) => {
            if let Some(typ) = typ.clone() {
                Ok(r#type::Type::Handle(typ))
            } else {
                Err("Missing type for Handle".to_string())
            }
        }
        TypeAnnotatedValue::Variant(variant) => {
            if let Some(typ) = variant.clone().typ {
                Ok(r#type::Type::Variant(typ))
            } else {
                Err("Missing type for Variant".to_string())
            }
        }
    };

    r#type.map(|r#type| Type {
        r#type: Some(r#type),
    })
}

pub fn get_value(value: TypeAnnotatedValue) -> Result<Value, String> {
    match value {
        TypeAnnotatedValue::Bool(value) => Ok(Value::Bool(value)),
        TypeAnnotatedValue::S8(value) => Ok(Value::S8(value as i8)),
        TypeAnnotatedValue::U8(value) => Ok(Value::U8(value as u8)),
        TypeAnnotatedValue::S16(value) => Ok(Value::S16(value as i16)),
        TypeAnnotatedValue::U16(value) => Ok(Value::U16(value as u16)),
        TypeAnnotatedValue::S32(value) => Ok(Value::S32(value)),
        TypeAnnotatedValue::U32(value) => Ok(Value::U32(value)),
        TypeAnnotatedValue::S64(value) => Ok(Value::S64(value)),
        TypeAnnotatedValue::U64(value) => Ok(Value::U64(value)),
        TypeAnnotatedValue::F32(value) => Ok(Value::F32(value)),
        TypeAnnotatedValue::F64(value) => Ok(Value::F64(value)),
        TypeAnnotatedValue::Char(value) => char::from_u32(value as u32)
            .map(Value::Char)
            .ok_or_else(|| "Invalid char value".to_string()),
        TypeAnnotatedValue::Str(value) => Ok(Value::String(value)),
        TypeAnnotatedValue::List(TypedList { typ: _, values }) => {
            let mut list_values = Vec::new();
            for value in values {
                let type_annotated_value = value
                    .type_annotated_value
                    .ok_or_else(|| "Missing type_annotated_value in List".to_string())?;
                list_values.push(get_value(type_annotated_value)?);
            }
            Ok(Value::List(list_values))
        }
        TypeAnnotatedValue::Tuple(TypedTuple { typ: _, value }) => {
            let mut tuple_values = Vec::new();
            for value in value {
                let type_annotated_value = value
                    .type_annotated_value
                    .ok_or_else(|| "Missing type_annotated_value in Tuple".to_string())?;
                tuple_values.push(get_value(type_annotated_value)?);
            }
            Ok(Value::Tuple(tuple_values))
        }
        TypeAnnotatedValue::Record(TypedRecord { typ: _, value }) => {
            let mut record_values = Vec::new();
            for name_value in value {
                let type_annotated_value = name_value
                    .value
                    .ok_or_else(|| "Missing value in Record".to_string())?
                    .type_annotated_value
                    .ok_or_else(|| "Missing type_annotated_value in Record".to_string())?;
                record_values.push(get_value(type_annotated_value)?);
            }
            Ok(Value::Record(record_values))
        }
        TypeAnnotatedValue::Flags(TypedFlags { typ, values }) => {
            let mut bools = Vec::new();
            for expected_flag in typ {
                bools.push(values.contains(&expected_flag));
            }
            Ok(Value::Flags(bools))
        }
        TypeAnnotatedValue::Enum(TypedEnum { typ, value }) => typ
            .iter()
            .position(|expected_enum| expected_enum == &value)
            .map(|index| Value::Enum(index as u32))
            .ok_or_else(|| "Enum value not found in the list of expected enums".to_string()),
        TypeAnnotatedValue::Option(option) => match option.value {
            Some(value) => {
                let type_annotated_value = value
                    .type_annotated_value
                    .ok_or_else(|| "Missing type_annotated_value in Option".to_string())?;
                let value = get_value(type_annotated_value)?;
                Ok(Value::Option(Some(Box::new(value))))
            }
            None => Ok(Value::Option(None)),
        },
        TypeAnnotatedValue::Result(result) => {
            let value = result
                .result_value
                .ok_or_else(|| "Missing value in Result".to_string())?;

            match value {
                ResultValue::OkValue(ok_value) => {
                    let type_annotated_value = ok_value
                        .type_annotated_value
                        .ok_or_else(|| "Missing type_annotated_value in Result Ok".to_string())?;
                    let value = get_value(type_annotated_value)?;
                    Ok(Value::Result(Ok(Some(Box::new(value)))))
                }

                ResultValue::ErrorValue(err_value) => {
                    let type_annotated_value = err_value
                        .type_annotated_value
                        .ok_or_else(|| "Missing type_annotated_value in Result Err".to_string())?;
                    let value = get_value(type_annotated_value)?;
                    Ok(Value::Result(Err(Some(Box::new(value)))))
                }
            }
        }
        TypeAnnotatedValue::Handle(handle) => Ok(Value::Handle {
            uri: Uri { value: handle.uri },
            resource_id: handle.resource_id,
        }),
        TypeAnnotatedValue::Variant(variant) => {
            let case_value = variant.case_value;
            let case_name = variant.case_name;
            let typ = variant
                .typ
                .ok_or_else(|| "Missing typ in Variant".to_string())?
                .cases
                .iter()
                .map(|nt| (nt.name.clone(), nt.typ.clone()))
                .collect::<Vec<_>>();

            let case_idx = typ
                .iter()
                .position(|(name, _)| name == &case_name)
                .ok_or_else(|| "Case name not found in Variant".to_string())?
                as u32;

            match case_value {
                Some(value) => {
                    let type_annotated_value = value
                        .type_annotated_value
                        .ok_or_else(|| "Missing type_annotated_value in Variant".to_string())?;
                    let result = get_value(type_annotated_value)?;
                    Ok(Value::Variant {
                        case_idx,
                        case_value: Some(Box::new(result)),
                    })
                }
                None => Ok(Value::Variant {
                    case_idx,
                    case_value: None,
                }),
            }
        }
    }
}

pub fn get_wit_value(value: TypeAnnotatedValue) -> Result<WitValue, String> {
    let value: Result<Value, String> = get_value(value);
    value.map(|v| v.into())
}
