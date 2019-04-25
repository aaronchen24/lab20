
type image = int list list ;;
type size = int * int ;;

open Graphics ;;
  
(* threshold thershold image -- image where pixels above the threshold
value are black *)
let threshold threshold img =
  List.map  (fun row -> List.map (fun v -> if v <= threshold then 0 else 1)
				 row) img
       
(* show the image where the maximum pixel value is max *)
let depict img max =
  Graphics.open_graph ""; Graphics.clear_graph ();
  let x, y = List.length (List.hd img), List.length img in Graphics.resize_window x y;
  let depict_pix v r c = let lvl = (255 * v) / max in Graphics.set_color (Graphics.rgb lvl lvl lvl);
  plot c (y - r) in
  List.iteri (fun r row ->
  List.iteri (fun c pix -> depict_pix pix r c) row) img;
  Unix.sleep 2; Graphics.close_graph () ;;

(* dither max image -- dithered image, max is the maximum value in the image. *)
let dither max img =
  List.map
    (fun row ->
     List.map
       (fun v -> if v > Random.int (max+1)
		 then 1
		 else 0) row)
    img
  
let mona = Monalisa.image ;;
  
  depict mona 255 ;;
    
  let mona_dither = dither 255 mona ;;
    depict mona_dither 1 ;;
      
    let mona_threshold = threshold 64 mona ;;
      depict mona_threshold 1 ;;
