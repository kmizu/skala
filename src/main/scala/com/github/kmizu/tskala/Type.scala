package com.github.kmizu.tskala

enum Type {
  case TInt
  case TBool
  case TString
  case TList(elementType: Type)
}
