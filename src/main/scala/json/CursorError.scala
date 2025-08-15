package com.github.aminmal
package json

enum CursorError {
  case PathDoesNotExist(path: String)
  case NotAnObject(path: String)
  case IndexOutOfBounds(path: String, arrSize: Int)
  case NotAnArray(path: String)
}
