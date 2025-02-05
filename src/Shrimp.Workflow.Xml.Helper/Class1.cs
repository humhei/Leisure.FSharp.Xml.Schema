using System;
using System.Xml.Serialization;
using System.Xml;

namespace Shrimp.Workflow.Xml.Helper
{
    public class Class1
    {
        void SerializePrimitive(XmlWriter writer, object o, XmlSerializerNamespaces namespaces)
        {
            var primitiveType = o.GetType();
            writer.Init(writer, namespaces, null, null, null);
            switch (Type.GetTypeCode(primitiveType))
            {
                case TypeCode.String:
                    writer.Write_string(o);
                    break;
                case TypeCode.Int32:
                    writer.Write_int(o);
                    break;
                case TypeCode.Boolean:
                    writer.Write_boolean(o);
                    break;
                case TypeCode.Int16:
                    writer.Write_short(o);
                    break;
                case TypeCode.Int64:
                    writer.Write_long(o);
                    break;
                case TypeCode.Single:
                    writer.Write_float(o);
                    break;
                case TypeCode.Double:
                    writer.Write_double(o);
                    break;
                case TypeCode.Decimal:
                    writer.Write_decimal(o);
                    break;
                case TypeCode.DateTime:
                    writer.Write_dateTime(o);
                    break;
                case TypeCode.Char:
                    writer.Write_char(o);
                    break;
                case TypeCode.Byte:
                    writer.Write_unsignedByte(o);
                    break;
                case TypeCode.SByte:
                    writer.Write_byte(o);
                    break;
                case TypeCode.UInt16:
                    writer.Write_unsignedShort(o);
                    break;
                case TypeCode.UInt32:
                    writer.Write_unsignedInt(o);
                    break;
                case TypeCode.UInt64:
                    writer.Write_unsignedLong(o);
                    break;

                default:
                    if (primitiveType == typeof(XmlQualifiedName))
                    {
                        writer.Write_QName(o);
                    }
                    else if (primitiveType == typeof(byte[]))
                    {
                        writer.Write_base64Binary(o);
                    }
                    else if (primitiveType == typeof(Guid))
                    {
                        writer.Write_guid(o);
                    }
                    else if (primitiveType == typeof(TimeSpan))
                    {
                        writer.Write_TimeSpan(o);
                    }
                    else
                    {
                        throw new InvalidOperationException(Res.GetString(Res.XmlUnxpectedType, primitiveType.FullName));
                    }
                    break;
            }
        }
    }
}
