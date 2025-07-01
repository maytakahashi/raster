open Core

(* You need to modify this function to blur the input image
   based on the provided radius instead of ignoring it. *)
let transform image ~radius =
  Image.mapi image ~f:(fun ~x ~y _pixel ->
    let slice =
      Image.slice
        image
        ~x_start:(max 0 (x - radius))
        ~x_end:(min ((Image.width image) - 1) (x + radius))
        ~y_start:(max 0 (y - radius))
        ~y_end:(min ((Image.height image) - 1) (y + radius))
    in
    Image.mean_pixel slice)
;;

let%expect_test "transform" =
  let output =
    transform (Image.load_ppm ~filename:"../images/beach_portrait.ppm") ~radius:3
  in
  let expected =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_blur.ppm"
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
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
