open Core

(* This should look familiar by now! *)
let transform copy ~n =
  let max = Float.of_int (Image.max_val copy) in
  let copy = Image.copy copy in
  Image.mapi copy ~f:(fun ~x ~y (red, green, blue) ->
    let rounded_value color =  
      let n_fl = Float.of_int (n-1) in
      let norm_rounded = Float.round ((Float.of_int color) /. max *. n_fl) in
      Int.of_float (norm_rounded /. n_fl *. max)
    in
    let result = (rounded_value red, rounded_value green, rounded_value blue)
    in
    let red_error = Float.of_int red -. Float.of_int (Pixel.red result) in
    let green_error = Float.of_int green -. Float.of_int (Pixel.green result) in
    let blue_error = Float.of_int blue -. Float.of_int (Pixel.blue result) in
    let y_in_range = y < Image.height copy - 1 in
    let x_in_range = x < Image.width copy - 1 in
    let adjust x_adj y_adj buffer =
      Image.set
        copy
        ~x:x_adj
        ~y:y_adj
        (let red, green, blue = Image.get copy ~x:x_adj ~y:y_adj in
         ( red + Int.of_float (Float.round (red_error *. buffer))
         , green + Int.of_float (Float.round (green_error *. buffer))
         , blue + Int.of_float (Float.round (blue_error *. buffer))))
    in
    if x > 0 && y_in_range then adjust (x - 1) (y + 1) (3. /. 16.);
    if x_in_range then adjust (x + 1) y (7. /. 16.);
    if y_in_range then adjust x (y + 1) (5. /. 16.);
    if x_in_range && y_in_range then adjust (x + 1) (y + 1) (1. /. 16.);
    result)
;;

let%expect_test "transform" =
  let output =
    transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm") ~n:2
  in
  let expected =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_dither_color.ppm"
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
      and n =
        flag
          "n"
          (required Command.Param.int)
          ~doc:"N the number of colors per channel"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~n in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
