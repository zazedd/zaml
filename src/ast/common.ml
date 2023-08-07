type variable = string

type pos = {
  fname : string; (* file name *)
  lnum : int; (* line number *)
  start : int; (* beginning line offset *)
  ending : int; (* position offset *)
}
