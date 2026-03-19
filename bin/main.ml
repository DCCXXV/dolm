open Tsdl
open Tsdl_ttf

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
let btn_h = 36
let btn_y0 = rpanel_y + grid_h - rpad - (3 * btn_h + 2 * btn_gap)

let state = Array.make (grid_size * grid_size) false
let selected = ref (-1, -1)
let steps = ref 0

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

let draw_text renderer font text cx cy =
    let color = Sdl.Color.create ~r:255 ~g:255 ~b:255 ~a:255 in
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

let draw_dolm renderer font =
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
    for row = 0 to 1 do
        for col = 0 to 1 do
            let idx = row * 2 + col in
            let (r, g, b) = btn_colors.(idx) in
            let bx = preview_x + col * (btn_w + btn_gap) in
            let by = btn_y0 + row * (btn_h + btn_gap) in
            ignore (Sdl.set_render_draw_color renderer r g b 255);
            draw_filled_rect renderer bx by btn_w btn_h
        done
    done;
    let (r, g, b) = btn_colors.(4) in
    ignore (Sdl.set_render_draw_color renderer r g b 255);
    draw_filled_rect renderer preview_x (btn_y0 + 2 * (btn_h + btn_gap)) (btn_w * 2 + btn_gap) btn_h;
    (* grid state *)
    ignore (Sdl.set_render_draw_color renderer 255 255 255 255);
    for i = 0 to Array.length state - 1 do
        if state.(i) then
            let (x, y) = itoxy i in
            ignore (Sdl.render_fill_rect renderer (Some (Sdl.Rect.create ~x:(grid_x + x*tile_size+1) ~y:(grid_y + y*tile_size+1) ~w:(tile_size-1) ~h:(tile_size-1))));
    done;
    (* selected *)
    ignore (Sdl.set_render_draw_color renderer 128 128 128 128);
    ignore (let x, y = !selected in Sdl.render_fill_rect renderer (Some (Sdl.Rect.create ~x:(grid_x + x*tile_size+1) ~y:(grid_y + y*tile_size+1) ~w:(tile_size-1) ~h:(tile_size-1))));
    (* panel borders *)
    ignore (Sdl.set_render_draw_color renderer 255 255 255 255);
    draw_rect_border renderer grid_x grid_y grid_w grid_h;
    draw_rect_border renderer rpanel_x rpanel_y rpanel_w grid_h;
    draw_rect_border renderer bpanel_x bpanel_y (window_w - 2 * margin) bpanel_h;
    draw_rect_border renderer preview_x preview_y preview_size preview_size;
    (* steps counter *)
    draw_text renderer font (string_of_int !steps) (preview_x + preview_size / 2) (preview_y + preview_size / 2)


(* logic *)

type shape = Dot | Dot4 | Plus | LPlus

let load_level level =
    Array.iteri (fun y row ->
       	for x = 0 to grid_size - 1 do
            state.(xytoi x y) <- row.[x * 2] = 'x';
        done
    ) level

let toggle x y =
	if x >= 0 && x < grid_size && y >= 0 && y < grid_size then state.(xytoi x y) <- not state.(xytoi x y)

let spell x y s =
    if x >= 0 && x < grid_size && y >= 0 && y < grid_size then begin
        (match s with
        | Dot  -> toggle x y
        | Dot4 -> toggle (x+1) (y+1); toggle (x+1) (y-1); toggle (x-1) (y+1); toggle (x-1) (y-1);
        | Plus -> toggle x y; toggle (x-1) y; toggle (x+1) y; toggle x (y-1); toggle x (y+1)
        | LPlus  -> for cy = 0 to grid_size - 1 do toggle x cy done;for cx = 0 to grid_size - 1 do toggle cx y done; toggle x y);
        incr steps
    end


let main () = match Sdl.init Sdl.Init.(video + events) with
| Error (`Msg e) -> Sdl.log "Init error: %s" e; 1
| Ok () ->
    ignore (Ttf.init ());
    match Ttf.open_font "assets/font/Micro5-Regular.ttf" 48 with
    | Error (`Msg e) -> Sdl.log "Font error: %s" e; -1
    | Ok font ->
    match Sdl.create_window ~w:window_w ~h:window_h "Dolm" Sdl.Window.shown with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; -1
    | Ok w ->
        match Sdl.create_renderer w with
        | Error (`Msg e) -> Sdl.log "Create renderer error: %s" e; -1
        | Ok renderer ->
        	ignore (Sdl.set_render_draw_blend_mode renderer Sdl.Blend.mode_blend);
            ignore (Sdl.render_set_logical_size renderer window_w window_h);
            load_level Levels.level1;
            let e = Sdl.Event.create () in
            (* event loop *)
            let rec loop () =
                while Sdl.poll_event (Some e) do
                    let typ = Sdl.Event.(get e typ) in
                    if typ = Sdl.Event.quit then raise Exit
                    else if Sdl.Event.(get e typ) = Sdl.Event.mouse_button_down then begin
                    let mx = Sdl.Event.(get e mouse_button_x) in
                        let my = Sdl.Event.(get e mouse_button_y) in
                        if mx >= grid_x && mx < grid_x + grid_w && my >= grid_y && my < grid_y + grid_h then begin
                            let x = (mx - grid_x) / tile_size in
                            let y = (my - grid_y) / tile_size in
                            selected := (x, y)
                        end else begin
                            (* spell buttons *)
                            let sx, sy = !selected in
                            for row = 0 to 1 do for col = 0 to 1 do
                                let idx = row * 2 + col in
                                let bx = preview_x + col * (btn_w + btn_gap) in
                                let by = btn_y0 + row * (btn_h + btn_gap) in
                                if mx >= bx && mx < bx + btn_w && my >= by && my < by + btn_h then
                                    spell sx sy [|Dot; Dot4; Plus; LPlus|].(idx)
                            done done;
                            (* reset button *)
                            let ry = btn_y0 + 2 * (btn_h + btn_gap) in
                            if mx >= preview_x && mx < preview_x + btn_w * 2 + btn_gap
                            && my >= ry && my < ry + btn_h then begin
                                load_level Levels.level1; selected := (-1, -1); steps := 0
                            end
                        end
                    end
                done;
                draw_dolm renderer font;
                Sdl.render_present renderer;
                Sdl.delay 16l;
                loop ()
            in
            (try loop () with Exit -> ());
            Ttf.close_font font;
            Sdl.destroy_renderer renderer;
            Sdl.destroy_window w;
            Ttf.quit ();
            Sdl.quit ();
            0

let () = if !Sys.interactive then () else exit (main ())
