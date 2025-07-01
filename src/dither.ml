open Core

(* This should look familiar by now! *)
let transform image =
  let gray = Grayscale.transform image in
  let max = Image.max_val gray in
  Image.mapi gray ~f:(fun ~x ~y (red, _green, _blue) ->
    let darkness = red in
    let result =
      if Float.( >=. ) (Float.of_int darkness *. 2.) (Float.of_int max)
      then max, max, max
      else 0, 0, 0
    in
    let error = Float.of_int darkness -. Float.of_int (Pixel.red result) in
    let y_in_range = y < Image.height image - 1 in
    let x_in_range = x < Image.width image - 1 in
    let adjust x_adj y_adj buffer =
      Image.set
        gray
        ~x:x_adj
        ~y:y_adj
        (let red, green, blue = Image.get gray ~x:x_adj ~y:y_adj in
         ( red + Int.of_float (Float.round buffer)
         , green + Int.of_float (Float.round buffer)
         , blue + Int.of_float (Float.round buffer) ))
    in
    if x > 0 && y_in_range then adjust (x - 1) (y + 1) (error *. 3. /. 16.);
    if x_in_range then adjust (x + 1) y (error *. 7. /. 16.);
    if y_in_range then adjust x (y + 1) (error *. 5. /. 16.);
    if x_in_range && y_in_range
    then adjust (x + 1) (y + 1) (error *. 1. /. 16.);
    result)
;;

let%expect_test "transform" =
  let output =
    transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm")
  in
  let expected =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_dither.ppm"
  in
  print_endline
    (sprintf
       "Output is off by %d of %d pixels"
       (Image.compare output expected)
       (Image.height expected * Image.width expected));
  [%expect
    {|
    Output is off by 0 of 461600 pixels
  |}]
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
