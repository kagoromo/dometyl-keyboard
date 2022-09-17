open! Base
open! Scad_ml
open! Generator

let body_lookups =
  let offset = function
    | 2 -> 0., 3.5, -8. (* middle *)
    | 3 -> 1., -1., -1.5 (* ring *)
    | i when i >= 4 -> 0., -12., 7. (* pinky *)
    | 0 -> -1., 0., -2.
    | _ -> 0., 0., -3.
  and curve = function
    | 0 ->
      Curvature.(
        curve ~well:(spec ~radius:85. (Float.pi /. 12.) ~tilt:(Float.pi /. 24.)) ())
    | _ -> Curvature.(curve ~well:(spec ~radius:85. (Float.pi /. 12.)) ())
  and swing = function
    | 2 -> Float.pi /. -48.
    | 3 -> Float.pi /. -19.
    | 4 -> Float.pi /. -14.
    | _ -> 0.
  and splay _ = 0.
  and rows _ = 3 in
  Plate.Lookups.body ~offset ~curve ~splay ~swing ~rows ()

let thumb_lookups =
  let curve _ =
    Curvature.(curve ~fan:{ angle = Float.pi /. 12.5; radius = 85.; tilt = 0. } ())
  in
  Plate.Lookups.thumb ~curve ()

let plate_builder =
  Plate.make
    ~n_body_cols:5
    ~spacing:1.5
    ~tent:(Float.pi /. 10.)
    ~body_lookups
    ~thumb_lookups
    ~thumb_offset:(-1., -50., -6.)
    ~thumb_angle:Float.(0., pi /. -4.3, pi /. 6.)

let wall_builder plate =
  Walls.
    { body =
        auto_body
          ~west_lookup:(fun i -> if i = 0 then Eye else Yes)
          ~east_lookup:(fun _ -> Yes)
          ~n_facets:1
          ~n_steps:(`PerZ 6.)
          ~north_clearance:2.5
          ~south_clearance:2.5
          ~side_clearance:1.5
          plate
    ; thumb =
        auto_thumb
          ~south_lookup:(fun _ -> Yes)
          ~east_lookup:(fun _ -> No)
          ~west_lookup:(fun _ -> Eye)
          ~n_steps:(`Flat 4)
          ~north_clearance:3.
          ~south_clearance:3.
          ~side_clearance:3.
          plate
    }

let base_connector =
  Connect.closed
    ~body_steps:(`PerZ 6.)
    ~thumb_steps:(`Flat 4)
    ~east_link:(Connect.snake ~height:15. ())
    ~west_link:(Connect.straight ~height:15. ())

let plate_welder = Plate.column_joins
let ports_cutter = Splaytyl.ports_cutter

let build ?right_hand ?hotswap () =
  let keyhole = Mx.make_hole ?hotswap () in
  Case.make
    ?right_hand
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    keyhole
