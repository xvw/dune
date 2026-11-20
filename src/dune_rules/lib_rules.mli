open Import

val foreign_rules
  :  Foreign_library.t
  -> sctx:Super_context.t
  -> expander:Expander.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> unit Memo.t

val compile_context
  :  Library.t
  -> sctx:Super_context.t
  -> dir_contents:Dir_contents.t
  -> expander:Expander.t
  -> scope:Scope.t
  -> for_:Compilation_mode.t
  -> Compilation_context.t Memo.t

(** [compile_context_in_dir ~dir ~lib_name ~for_] is the compilation context
    of the enabled library named [lib_name] defined in [dir], for rules that
    live outside of that directory. The rules produced while computing it are
    discarded, as they are already set up by [rules] in [dir]. *)
val compile_context_in_dir
  :  dir:Path.Build.t
  -> lib_name:string
  -> for_:Compilation_mode.t
  -> (Library.t * Compilation_context.t) option Memo.t

val rules
  :  Library.t
  -> sctx:Super_context.t
  -> dir_contents:Dir_contents.t
  -> expander:Expander.t
  -> scope:Scope.t
  -> (Compilation_context.t * Merlin.t) option Compilation_mode.Per_mode.t Memo.t
