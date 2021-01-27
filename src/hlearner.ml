open Tactician_ltac1_record_plugin
open Tactic_learner
open Ltac_plugin
open Tacexpr
open Hammer_tactics
open Sauto

let get_tactic (s : string) =
  try
    (Tacenv.locate_tactic (Libnames.qualid_of_string s))
  with Not_found ->
    failwith ("tactic not found: " ^ s)

let get_tacexpr tac args =
  Tacexpr.TacArg(CAst.make
                   Tacexpr.(TacCall(CAst.make
                                      (Locus.ArgArg(None, get_tactic tac),
                                       args))))

let extra_tactic = sauto (default_s_opts ())

let () = Ltacrecord.register Ltacrecord.ml_record_tac "extratac"

let ltac_extra_tactic : glob_tactic_expr =
  TacML (CAst.make ({mltac_name = {mltac_plugin = "recording"; mltac_tactic = "extratac"}; mltac_index = 0},
                    []))

module HLearner : TacticianOnlineLearnerType = functor (TS : TacticianStructures) -> struct
  module LH = Learner_helper.L(TS)
  open TS
  open LH
  module LSHF = Lshf_learner.LSHF(TS)

  type model = LSHF.model

  let extra_tactic = {confidence = 1.; focus = 0; tactic = tactic_make
                                                    (TacTimeout ((Locus.ArgArg 1), (get_tacexpr "strivial" []))) }
  let empty = LSHF.empty
  let learn = LSHF.learn
  let predict m s =
    let res = LSHF.predict m s in
    IStream.cons extra_tactic res
  let evaluate db _ _ = 0., db
end

let () = register_online_learner "Hplugin learner" (module HLearner)
let _ = print_endline "boe"
