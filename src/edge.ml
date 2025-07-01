open Core

(* This should look familiar by now! *)
let transform image =
  let gray = Grayscale.transform image in
  let blur = Blur.transform gray ~radius:2 in
  let max = Image.max_val gray in
  Image.mapi blur ~f:(fun ~x ~y _pixel ->
    let get scale ~x_adj ~y_adj : int =
      scale * Pixel.red (Image.get blur ~x:x_adj ~y:y_adj)
    in
    let x_in_range num = 0 < x + num && x + num < Image.width blur in
    let y_in_range num = 0 < y + num && y + num < Image.height blur in
    let g_x =
      Float.of_int
        ((if y_in_range (-1) && x_in_range (-1)
          then get (-1) ~x_adj:(x - 1) ~y_adj:(y - 1)
          else 0)
         + (if x_in_range (-1) then get (-2) ~x_adj:(x - 1) ~y_adj:y else 0)
         + (if y_in_range 1 && x_in_range (-1)
            then get (-1) ~x_adj:(x - 1) ~y_adj:(y + 1)
            else 0)
         + (if y_in_range (-1) && x_in_range 1
            then get 1 ~x_adj:(x + 1) ~y_adj:(y - 1)
            else 0)
         + (if x_in_range 1 then get 2 ~x_adj:(x + 1) ~y_adj:y else 0)
         +
         if y_in_range 1 && x_in_range 1
         then get 1 ~x_adj:(x + 1) ~y_adj:(y + 1)
         else 0)
    in
    let g_y =
      Float.of_int
        ((if x_in_range (-1) && y_in_range (-1)
          then get (-1) ~x_adj:(x - 1) ~y_adj:(y - 1)
          else 0)
         + (if y_in_range (-1) then get (-2) ~x_adj:x ~y_adj:(y - 1) else 0)
         + (if x_in_range 1 && y_in_range (-1)
            then get (-1) ~x_adj:(x + 1) ~y_adj:(y - 1)
            else 0)
         + (if x_in_range (-1) && y_in_range 1
            then get 1 ~x_adj:(x - 1) ~y_adj:(y + 1)
            else 0)
         + (if y_in_range 1 then get 2 ~x_adj:x ~y_adj:(y + 1) else 0)
         +
         if x_in_range 1 && y_in_range 1
         then get 1 ~x_adj:(x + 1) ~y_adj:(y + 1)
         else 0)
    in
    let gradient_mag = Float.sqrt (Float.square g_x +. Float.square g_y) in
    if Float.( >. ) gradient_mag (Float.of_int max *. 0.4)
    then max, max, max
    else 0, 0, 0)
;;

let%expect_test "transform" =
  let output =
    transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm")
  in
  let expected =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_edge.ppm"
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
