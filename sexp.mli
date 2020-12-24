
type descr_item = {
  name: string;
  impl: string;
  intf: string;
  cmt: string;
  cmti: string;
}

val get_module_descrs : unit -> descr_item list
