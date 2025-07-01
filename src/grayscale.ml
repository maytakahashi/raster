open Core

(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)
let transform image =
  Image.map image ~f:(fun (red, green, blue) ->
    let avg = (red + green + blue) / 3 in
    avg, avg, avg)
;;

let%expect_test "transform" =
  let output =
    transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm")
  in
  let expected =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm"
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
    ~summary:"Convert an image to grayscale"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;
