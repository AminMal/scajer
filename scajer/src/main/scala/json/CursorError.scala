package scajer.json

enum CursorError {
  case PathDoesNotExist(path: String)
  case NotAnObject(path: String)
  case IndexOutOfBounds(path: String, arrSize: Int)
  case NotAnArray(path: String)
  case IncompatibleValueType(wanted: String, valueType: String, path: String)

  def toJsonException: JsonException = this match {
    case CursorError.PathDoesNotExist(path)             => JsonException(s"json path does not exist: $path")
    case CursorError.NotAnObject(path)                  => JsonException(s"value at `$path` is not an object")
    case CursorError.IndexOutOfBounds(path, arrSize)    => JsonException(s"json array index out of bounds.\n\t$path\n\tarray size: $arrSize")
    case CursorError.NotAnArray(path)                   => JsonException(s"value at `$path` is not an array")
    case IncompatibleValueType(wanted, valueType, path) =>
      JsonException(s"cannot cast json value of type $valueType (at path = $path) to type $wanted")
  }
}
