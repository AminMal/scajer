package scajer.json

enum CursorError {
  case PathDoesNotExist(path: String)
  case NotAnObject(path: String)
  case IndexOutOfBounds(path: String, arrSize: Int)
  case NotAnArray(path: String)
  case IncompatibleValueType(wanted: String, valueType: String, path: String)

  override def toString: String = this match {
    case CursorError.PathDoesNotExist(path)             => s"json path does not exist: $path"
    case CursorError.NotAnObject(path)                  => s"value at `$path` is not an object"
    case CursorError.IndexOutOfBounds(path, arrSize)    => s"json array index out of bounds.\n\t$path\n\tarray size: $arrSize"
    case CursorError.NotAnArray(path)                   => s"value at `$path` is not an array"
    case IncompatibleValueType(wanted, valueType, path) =>
      s"cannot cast json value of type $valueType (at path = $path) to type $wanted"
  }

  def toJsonException: JsonException = JsonException(this.toString)
}
