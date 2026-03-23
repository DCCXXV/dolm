open Tsdl
open Tsdl_ttf

type shape = Dot | Dot4 | Plus | LPlus

(* ui *)

let margin = 2

let rpanel_w = 240
let bpanel_h = 240

let grid_size = 8
let tile_size = 60
let grid_w = grid_size * tile_size
let grid_h = grid_size * tile_size

let grid_x = margin
let grid_y = margin
let rpanel_x = grid_x + grid_w + margin
let rpanel_y = margin
let bpanel_x = margin
let bpanel_y = grid_y + grid_h + margin

let window_w = rpanel_x + rpanel_w + margin
let window_h = bpanel_y + bpanel_h + margin

let rpad = 12
let preview_size = rpanel_w - 2 * rpad
let preview_x = rpanel_x + rpad
let preview_y = rpanel_y + rpad

let btn_gap = rpad
let btn_w = (rpanel_w - 2 * rpad - btn_gap) / 2
let btn_area_y0 = preview_y + preview_size + rpad
let btn_area_h = rpanel_y + grid_h - rpad - btn_area_y0
let reset_btn_h = btn_gap * 3
let btn_h = (btn_area_h - reset_btn_h - 2 * btn_gap) / 2
let btn_y0 = btn_area_y0

let lvl_cols = 6
let lvl_rows = 3
let lvl_gap = rpad
let lvl_btn_w = (window_w - 2 * margin - 2 * rpad - (lvl_cols - 1) * lvl_gap) / lvl_cols
let lvl_btn_h = (bpanel_h - 2 * rpad - (4 - 1) * lvl_gap) / 4
let lvl_total_w = lvl_cols * lvl_btn_w + (lvl_cols - 1) * lvl_gap
let lvl_x0 = bpanel_x + (window_w - 2 * margin - lvl_total_w) / 2
let lvl_y0 = bpanel_y + rpad

let state = Array.make (grid_size * grid_size) false
let hover = Array.make (grid_size * grid_size) false

let selected = ref Dot
let steps = ref 0
let current_level = ref 0

let xytoi x y = y * grid_size + x
let itoxy i = i mod grid_size, i/grid_size

let draw_rect_border renderer x y w h =
	ignore (Sdl.render_draw_rect renderer (Some (Sdl.Rect.create ~x ~y ~w ~h)))

let draw_filled_rect renderer x y w h =
	ignore (Sdl.render_fill_rect renderer (Some (Sdl.Rect.create ~x ~y ~w ~h)))

let btn_colors = [|
	(255,   0, 255);  (* dot   *)
	(255, 255,   0);  (* dot4  *)
	(255, 128,   0);  (* plus  *)
	(  0, 255, 255);  (* lplus *)
	(255, 255, 255);  (* reset *)
|]

let draw_text renderer font text cx cy r g b =
	let color = Sdl.Color.create ~r ~g ~b ~a:255 in
	match Ttf.render_text_solid font text color with
	| Error _ -> ()
	| Ok surface ->
		match Sdl.create_texture_from_surface renderer surface with
		| Error _ -> Sdl.free_surface surface
		| Ok texture ->
			Sdl.free_surface surface;
			(match Sdl.query_texture texture with
			| Error _ -> ()
			| Ok (_, _, (w, h)) ->
				let dst = Sdl.Rect.create ~x:(cx - w/2) ~y:(cy - h/2) ~w ~h in
				ignore (Sdl.render_copy renderer texture ~dst));
			Sdl.destroy_texture texture

let draw_shape_icon renderer bx by bw bh shape is_selected =
	let idx = match shape with Dot -> 0 | Dot4 -> 1 | Plus -> 2 | LPlus -> 3 in
	let (r, g, b) = btn_colors.(idx) in
	let (sr, sg, sb) = if is_selected then (0, 0, 0) else (r, g, b) in
	ignore (Sdl.set_render_draw_color renderer sr sg sb 255);
	let cell = (min bw bh) / 8 in
	let dot_s = cell in
	let ox = bx + (bw - 5 * cell) / 2 in
	let oy = by + (bh - 5 * cell) / 2 in
	let dot gx gy =
		let px = ox + gx * cell + (cell - dot_s) / 2 in
		let py = oy + gy * cell + (cell - dot_s) / 2 in
		draw_filled_rect renderer px py dot_s dot_s
	in
	(match shape with
	| Dot  -> dot 2 2
	| Dot4 -> dot 1 1; dot 3 1; dot 1 3; dot 3 3
	| Plus -> dot 2 2; dot 1 2; dot 3 2; dot 2 1; dot 2 3
	| LPlus -> for g = 0 to 4 do dot 2 g; dot g 2 done)

let draw_dolm renderer font small_font =
	(* background *)
	ignore (Sdl.set_render_draw_color renderer 0 0 0 255);
	ignore (Sdl.render_clear renderer);
	(* grid lines *)
	ignore (Sdl.set_render_draw_color renderer 255 255 255 255);
	for i = 1 to grid_size - 1 do
		let x = grid_x + i * tile_size in
		let y = grid_y + i * tile_size in
		ignore (Sdl.render_draw_line renderer x grid_y x (grid_y + grid_h - 1));
		ignore (Sdl.render_draw_line renderer grid_x y (grid_x + grid_w - 1) y);
	done;
	(* right panel buttons *)
	let shapes = [|Dot; Dot4; Plus; LPlus|] in
	for row = 0 to 1 do
		for col = 0 to 1 do
			let idx = row * 2 + col in
			let (r, g, b) = btn_colors.(idx) in
			let bx = preview_x + col * (btn_w + btn_gap) in
			let by = btn_y0 + row * (btn_h + btn_gap) in
			ignore (Sdl.set_render_draw_color renderer r g b 255);
			if !selected = shapes.(idx) then
				draw_filled_rect renderer bx by btn_w btn_h
			else
				draw_rect_border renderer bx by btn_w btn_h;
			draw_shape_icon renderer bx by btn_w btn_h shapes.(idx) (!selected = shapes.(idx))
		done
	done;
	let reset_y = btn_y0 + 2 * (btn_h + btn_gap) in
	ignore (Sdl.set_render_draw_color renderer 255 255 255 255);
	draw_filled_rect renderer preview_x reset_y (btn_w * 2 + btn_gap) reset_btn_h;
	draw_text renderer small_font "RESET" (preview_x + (btn_w * 2 + btn_gap) / 2) (reset_y + reset_btn_h / 2) 0 0 0;
	(* grid state *)
	ignore (Sdl.set_render_draw_color renderer 255 255 255 255);
	for i = 0 to Array.length state - 1 do
		if state.(i) then
			let (x, y) = itoxy i in
			ignore (Sdl.render_fill_rect renderer (Some (Sdl.Rect.create ~x:(grid_x + x*tile_size+1) ~y:(grid_y + y*tile_size+1) ~w:(tile_size-1) ~h:(tile_size-1))));
	done;
	(* grid hover *)
	ignore (Sdl.set_render_draw_color renderer 128 128 128 128);
	for i = 0 to Array.length hover - 1 do
		if hover.(i) then
			let (x, y) = itoxy i in
			ignore (Sdl.render_fill_rect renderer (Some (Sdl.Rect.create ~x:(grid_x + x*tile_size+1) ~y:(grid_y + y*tile_size+1) ~w:(tile_size-1) ~h:(tile_size-1))));
	done;
	(* panel borders *)
	ignore (Sdl.set_render_draw_color renderer 255 255 255 255);
	draw_rect_border renderer grid_x grid_y grid_w grid_h;
	draw_rect_border renderer rpanel_x rpanel_y rpanel_w grid_h;
	draw_rect_border renderer bpanel_x bpanel_y (window_w - 2 * margin) bpanel_h;
	draw_rect_border renderer preview_x preview_y preview_size preview_size;
	(* steps counter *)
	draw_text renderer font (string_of_int !steps) (preview_x + preview_size / 2) (preview_y + preview_size / 2) 255 255 255;
	(* level selector *)
	for i = 0 to Array.length Levels.all - 1 do
		let row = i / lvl_cols in
		let col = i mod lvl_cols in
		let bx = lvl_x0 + col * (lvl_btn_w + lvl_gap) in
		let by = lvl_y0 + row * (lvl_btn_h + lvl_gap) in
		let is_current = i = !current_level in
		if is_current then begin
			ignore (Sdl.set_render_draw_color renderer 255 255 255 255);
			draw_filled_rect renderer bx by lvl_btn_w lvl_btn_h
		end else begin
			ignore (Sdl.set_render_draw_color renderer 255 255 255 255);
			draw_rect_border renderer bx by lvl_btn_w lvl_btn_h
		end
	done;
	(* level title *)
	let title = Levels.all.(!current_level).title in
	let title_y = lvl_y0 + lvl_rows * (lvl_btn_h + lvl_gap) + lvl_btn_h / 2 in
	let title_cx = bpanel_x + (window_w - 2 * margin) / 2 in
	draw_text renderer small_font title title_cx title_y 255 255 255


(* logic *)

let load_level (lvl : Levels.level) =
	match lvl.grid with
	| None -> Array.fill state 0 (Array.length state) false
	| Some grid ->
		Array.iteri (fun y row ->
			for x = 0 to grid_size - 1 do
				state.(xytoi x y) <- row.[x * 2] = 'x'
			done
		) grid

let toggle_state x y =
	if x >= 0 && x < grid_size && y >= 0 && y < grid_size then state.(xytoi x y) <- not state.(xytoi x y)

let spell x y s =
	if x >= 0 && x < grid_size && y >= 0 && y < grid_size then begin
		(match s with
		| Dot  -> toggle_state x y
		| Dot4 -> toggle_state (x+1) (y+1); toggle_state (x+1) (y-1); toggle_state (x-1) (y+1); toggle_state (x-1) (y-1);
		| Plus -> toggle_state x y; toggle_state (x-1) y; toggle_state (x+1) y; toggle_state x (y-1); toggle_state x (y+1)
		| LPlus  -> for cy = 0 to grid_size - 1 do toggle_state x cy done;for cx = 0 to grid_size - 1 do toggle_state cx y done; toggle_state x y);
		incr steps
	end

let clear_hover h =
	for y = 0 to grid_size - 1 do
		for x = 0 to grid_size - 1 do
			h.(xytoi x y) <- false;
		done
	done

let toggle_hover x y =
	if x >= 0 && x < grid_size && y >= 0 && y < grid_size then hover.(xytoi x y) <- true

let do_hover x y s =
	if x >= 0 && x < grid_size && y >= 0 && y < grid_size then begin
		clear_hover hover;
		(match s with
		| Dot  -> toggle_hover x y
		| Dot4 -> toggle_hover (x+1) (y+1); toggle_hover (x+1) (y-1); toggle_hover (x-1) (y+1); toggle_hover (x-1) (y-1);
		| Plus -> toggle_hover x y; toggle_hover (x-1) y; toggle_hover (x+1) y; toggle_hover x (y-1); toggle_hover x (y+1)
		| LPlus  -> for cy = 0 to grid_size - 1 do toggle_hover x cy done;for cx = 0 to grid_size - 1 do toggle_hover cx y done; toggle_hover x y);
	end


let main () = match Sdl.init Sdl.Init.(video + events) with
| Error (`Msg e) -> Sdl.log "Init error: %s" e; 1
| Ok () ->
	ignore (Ttf.init ());
	match Ttf.open_font "assets/font/Micro5-Regular.ttf" 72 with
	| Error (`Msg e) -> Sdl.log "Font error: %s" e; -1
	| Ok font ->
	match Ttf.open_font "assets/font/Micro5-Regular.ttf" 48 with
	| Error (`Msg e) -> Sdl.log "Font error: %s" e; -1
	| Ok small_font ->
	match Sdl.create_window ~w:window_w ~h:window_h "Dolm" Sdl.Window.(shown + resizable) with
	| Error (`Msg e) -> Sdl.log "Create window error: %s" e; -1
	| Ok w ->
		match Sdl.create_renderer w with
		| Error (`Msg e) -> Sdl.log "Create renderer error: %s" e; -1
		| Ok renderer ->
			ignore (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend);
			ignore (Sdl.render_set_logical_size renderer window_w window_h);
			load_level Levels.all.(0);
			let e = Sdl.Event.create () in
			let rec loop () =
				while Sdl.poll_event (Some e) do
					let typ = Sdl.Event.(get e typ) in
					if typ = Sdl.Event.quit then raise Exit
					else if typ = Sdl.Event.mouse_button_down then begin
						let mx = Sdl.Event.(get e mouse_button_x) in
						let my = Sdl.Event.(get e mouse_button_y) in
						if mx >= grid_x && mx < grid_x + grid_w && my >= grid_y && my < grid_y + grid_h then begin
							let x = (mx - grid_x) / tile_size in
							let y = (my - grid_y) / tile_size in
							spell x y !selected
						end else begin
							(* spell buttons *)
							for row = 0 to 1 do for col = 0 to 1 do
								let idx = row * 2 + col in
								let bx = preview_x + col * (btn_w + btn_gap) in
								let by = btn_y0 + row * (btn_h + btn_gap) in
								if mx >= bx && mx < bx + btn_w && my >= by && my < by + btn_h then
									selected := [|Dot; Dot4; Plus; LPlus|].(idx)
							done done;
							(* reset button *)
							let ry = btn_y0 + 2 * (btn_h + btn_gap) in
							if mx >= preview_x && mx < preview_x + btn_w * 2 + btn_gap
							&& my >= ry && my < ry + reset_btn_h then begin
								load_level Levels.all.(!current_level); steps := 0
							end;
							(* level buttons *)
							for i = 0 to Array.length Levels.all - 1 do
								let row = i / lvl_cols in
								let col = i mod lvl_cols in
								let bx = lvl_x0 + col * (lvl_btn_w + lvl_gap) in
								let by = lvl_y0 + row * (lvl_btn_h + lvl_gap) in
								if mx >= bx && mx < bx + lvl_btn_w && my >= by && my < by + lvl_btn_h then begin
									current_level := i;
									load_level Levels.all.(i);
									steps := 0
								end
							done
						end
					end
					else if typ = Sdl.Event.mouse_motion then begin
						let mx = Sdl.Event.(get e mouse_motion_x) in
						let my = Sdl.Event.(get e mouse_motion_y) in
						if mx >= grid_x && mx < grid_x + grid_w && my >= grid_y && my < grid_y + grid_h then begin
							let x = (mx - grid_x) / tile_size in
							let y = (my - grid_y) / tile_size in
							do_hover x y !selected
						end else
							clear_hover hover
					end
				done;
				draw_dolm renderer font small_font;
				Sdl.render_present renderer;
				Sdl.delay 16l;
				loop ()
			in
			(try loop () with Exit -> ());
			Ttf.close_font font;
			Ttf.close_font small_font;
			Sdl.destroy_renderer renderer;
			Sdl.destroy_window w;
			Ttf.quit ();
			Sdl.quit ();
			0

let () = if !Sys.interactive then () else exit (main ())
