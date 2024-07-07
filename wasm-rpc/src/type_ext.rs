use crate::protobuf::{
    r#type, NameOptionTypePair, NameTypePair, PrimitiveType, Type, TypeEnum, TypeFlags, TypeHandle,
    TypeList, TypeOption, TypePrimitive, TypeRecord, TypeResult, TypeTuple, TypeVariant,
};
use golem_wasm_ast::analysis::{AnalysedResourceId, AnalysedResourceMode, AnalysedType};
use std::ops::Deref;

pub trait TypeExt {
    fn to_type(&self) -> Type;
    fn from_type(typ: &Type) -> Result<Self, String>
    where
        Self: Sized;
}

impl TypeExt for AnalysedType {
    fn to_type(&self) -> Type {
        let r#type = match self {
            AnalysedType::Bool => Some(r#type::Type::Primitive(TypePrimitive {
                primitive: PrimitiveType::Bool as i32,
            })),
            AnalysedType::S8 => Some(r#type::Type::Primitive(TypePrimitive {
                primitive: PrimitiveType::S8 as i32,
            })),
            AnalysedType::U8 => Some(r#type::Type::Primitive(TypePrimitive {
                primitive: PrimitiveType::U8 as i32,
            })),
            AnalysedType::S16 => Some(r#type::Type::Primitive(TypePrimitive {
                primitive: PrimitiveType::S16 as i32,
            })),
            AnalysedType::U16 => Some(r#type::Type::Primitive(TypePrimitive {
                primitive: PrimitiveType::U16 as i32,
            })),
            AnalysedType::S32 => Some(r#type::Type::Primitive(TypePrimitive {
                primitive: PrimitiveType::S32 as i32,
            })),
            AnalysedType::U32 => Some(r#type::Type::Primitive(TypePrimitive {
                primitive: PrimitiveType::U32 as i32,
            })),
            AnalysedType::S64 => Some(r#type::Type::Primitive(TypePrimitive {
                primitive: PrimitiveType::S64 as i32,
            })),
            AnalysedType::U64 => Some(r#type::Type::Primitive(TypePrimitive {
                primitive: PrimitiveType::U64 as i32,
            })),
            AnalysedType::F32 => Some(r#type::Type::Primitive(TypePrimitive {
                primitive: PrimitiveType::F32 as i32,
            })),
            AnalysedType::F64 => Some(r#type::Type::Primitive(TypePrimitive {
                primitive: PrimitiveType::F64 as i32,
            })),
            AnalysedType::Chr => Some(r#type::Type::Primitive(TypePrimitive {
                primitive: PrimitiveType::Chr as i32,
            })),
            AnalysedType::Str => Some(r#type::Type::Primitive(TypePrimitive {
                primitive: PrimitiveType::Str as i32,
            })),
            AnalysedType::List(inner) => Some(r#type::Type::List(Box::new(TypeList {
                elem: Some(Box::new(inner.to_type())),
            }))),
            AnalysedType::Tuple(elems) => Some(r#type::Type::Tuple(TypeTuple {
                elems: elems
                    .iter()
                    .map(|analysed_type| analysed_type.to_type())
                    .collect(),
            })),
            AnalysedType::Record(fields) => Some(r#type::Type::Record(TypeRecord {
                fields: fields
                    .iter()
                    .map(|(name, analysed_type)| NameTypePair {
                        name: name.clone(),
                        typ: Some(analysed_type.to_type()),
                    })
                    .collect(),
            })),
            AnalysedType::Flags(names) => Some(r#type::Type::Flags(TypeFlags {
                names: names.clone(),
            })),
            AnalysedType::Enum(names) => Some(r#type::Type::Enum(TypeEnum {
                names: names.clone(),
            })),
            AnalysedType::Option(inner) => Some(r#type::Type::Option(Box::new(TypeOption {
                elem: Some(Box::new(inner.to_type())),
            }))),
            AnalysedType::Result { ok, error } => {
                Some(r#type::Type::Result(Box::new(TypeResult {
                    ok: ok.clone().map(|ok_type| Box::new(ok_type.to_type())),
                    err: error.clone().map(|err_type| Box::new(err_type.to_type())),
                })))
            }
            AnalysedType::Variant(cases) => Some(r#type::Type::Variant(TypeVariant {
                cases: cases
                    .iter()
                    .map(|(name, typ)| NameOptionTypePair {
                        name: name.clone(),
                        typ: typ.clone().map(|analysed_type| analysed_type.to_type()),
                    })
                    .collect(),
            })),
            AnalysedType::Resource { id, resource_mode } => {
                Some(r#type::Type::Handle(TypeHandle {
                    resource_id: id.value,
                    mode: match resource_mode {
                        AnalysedResourceMode::Owned => 0,
                        AnalysedResourceMode::Borrowed => 1,
                    },
                }))
            }
        };

        Type { r#type }
    }

    fn from_type(typ: &Type) -> Result<Self, String> {
        match &typ.r#type {
            Some(r#type::Type::Primitive(primitive)) => {
                match PrimitiveType::try_from(primitive.primitive) {
                    Ok(PrimitiveType::Bool) => Ok(AnalysedType::Bool),
                    Ok(PrimitiveType::S8) => Ok(AnalysedType::S8),
                    Ok(PrimitiveType::U8) => Ok(AnalysedType::U8),
                    Ok(PrimitiveType::S16) => Ok(AnalysedType::S16),
                    Ok(PrimitiveType::U16) => Ok(AnalysedType::U16),
                    Ok(PrimitiveType::S32) => Ok(AnalysedType::S32),
                    Ok(PrimitiveType::U32) => Ok(AnalysedType::U32),
                    Ok(PrimitiveType::S64) => Ok(AnalysedType::S64),
                    Ok(PrimitiveType::U64) => Ok(AnalysedType::U64),
                    Ok(PrimitiveType::F32) => Ok(AnalysedType::F32),
                    Ok(PrimitiveType::F64) => Ok(AnalysedType::F64),
                    Ok(PrimitiveType::Chr) => Ok(AnalysedType::Chr),
                    Ok(PrimitiveType::Str) => Ok(AnalysedType::Str),
                    Err(_) => Err("Unknown primitive type".to_string()),
                }
            }
            Some(r#type::Type::List(inner)) => {
                let elem_type = inner
                    .elem
                    .as_ref()
                    .ok_or_else(|| "List element type is None".to_string())?;
                let analysed_type = AnalysedType::from_type(elem_type)?;
                Ok(AnalysedType::List(Box::new(analysed_type)))
            }
            Some(r#type::Type::Tuple(inner)) => {
                let elems = inner
                    .elems
                    .iter()
                    .map(AnalysedType::from_type)
                    .collect::<Result<Vec<_>, String>>()?;
                Ok(AnalysedType::Tuple(elems))
            }
            Some(r#type::Type::Record(inner)) => {
                let fields = inner
                    .fields
                    .iter()
                    .map(|field| {
                        let field_type = field
                            .typ
                            .as_ref()
                            .ok_or_else(|| format!("Record field {} type is None", field.name))?;
                        let analysed_type = AnalysedType::from_type(field_type)?;
                        Ok((field.name.clone(), analysed_type))
                    })
                    .collect::<Result<Vec<_>, String>>()?;
                Ok(AnalysedType::Record(fields))
            }
            Some(r#type::Type::Flags(inner)) => Ok(AnalysedType::Flags(inner.names.clone())),
            Some(r#type::Type::Enum(inner)) => Ok(AnalysedType::Enum(inner.names.clone())),
            Some(r#type::Type::Option(inner)) => {
                let elem_type = inner
                    .elem
                    .as_ref()
                    .ok_or_else(|| "Option element type is None".to_string())?;
                let analysed_type = AnalysedType::from_type(elem_type)?;
                Ok(AnalysedType::Option(Box::new(analysed_type)))
            }
            Some(r#type::Type::Result(inner)) => {
                let ok_type = inner
                    .ok
                    .as_ref()
                    .map(|tpe| AnalysedType::from_type(tpe.deref()))
                    .transpose()?;
                let err_type = inner
                    .err
                    .as_ref()
                    .map(|tpe| AnalysedType::from_type(tpe.deref()))
                    .transpose()?;
                Ok(AnalysedType::Result {
                    ok: ok_type.map(Box::new),
                    error: err_type.map(Box::new),
                })
            }
            Some(r#type::Type::Variant(inner)) => {
                let cases = inner
                    .cases
                    .iter()
                    .map(|case| {
                        let case_type =
                            case.typ.as_ref().map(AnalysedType::from_type).transpose()?;
                        Ok((case.name.clone(), case_type))
                    })
                    .collect::<Result<Vec<_>, String>>()?;
                Ok(AnalysedType::Variant(cases))
            }
            Some(r#type::Type::Handle(inner)) => {
                let resource_mode = match inner.mode {
                    0 => Ok(AnalysedResourceMode::Owned),
                    1 => Ok(AnalysedResourceMode::Borrowed),
                    _ => Err("Invalid resource mode".to_string()),
                }?;
                Ok(AnalysedType::Resource {
                    id: AnalysedResourceId {
                        value: inner.resource_id,
                    },
                    resource_mode,
                })
            }
            None => Err("Type is None".to_string()),
        }
    }
}
