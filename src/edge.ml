open Core

(* This should look familiar by now! *)
let transform image =
  let gray = Grayscale.transform image in
  let blur = Blur.transform gray ~radius:2 in
  let max = Image.max_val gray in
  Image.mapi blur ~f:(fun ~x ~y _pixel ->
    let index_diff =
      [ -1, -1; 0, -1; 1, -1; -1, 0; 0, 0; 1, 0; -1, 1; 0, 1; 1, 1 ]
    in
    let pixel_values =
      List.map index_diff ~f:(fun (x_adj, y_adj) ->
      let x_in_range = 0 < x + x_adj && x + x_adj < Image.width blur in
      let y_in_range = 0 < y + y_adj && y + y_adj < Image.height blur in
      if x_in_range && y_in_range then 
        Pixel.red (Image.get blur ~x:(x + x_adj) ~y:(y + y_adj)) else 0)
    in
    let scale n pixel_index = n * List.nth_exn pixel_values (pixel_index-1) in
    let g_x =
      Float.of_int (scale (-1) 1 + scale (-2) 4 + scale (-1) 7 + scale 1 3 + scale 2 6 + scale 1 9)
    in
    let g_y =
      Float.of_int(scale (-1) 1 + scale (-2) 2 + scale (-1) 3 + scale 1 7 + scale 2 8 + scale 1 9)
    in
    let gradient_mag = Float.sqrt (Float.square g_x +. Float.square g_y) in
    if Float.( >. ) gradient_mag (Float.of_int max *. 0.4)
    then max, max, max
    else 0, 0, 0)
;;

let command =
  Command.basic
    ~summary:"edge an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edge.ppm")]
;;
