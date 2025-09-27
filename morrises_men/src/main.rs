use std::collections::HashMap;
use macroquad::prelude::*;

#[derive(Clone, Copy, PartialEq, Eq)]
enum PlayerType {
    Player1,
    Player2,
    None,
}

fn draw_board(p: f32, x_offset: f32, positions: &HashMap<String, (f32, f32)>, board_state: &mut HashMap<String, PlayerType>) {
    let color: Color = BROWN;

    draw_lines(x_offset, p, color);
    draw_markers(positions, p, color);
    draw_pieces(positions, board_state, p);

    fn draw_markers(positions: &HashMap<String, (f32, f32)>, p: f32, color: Color) {
        let r = p / 6.5;

        for (_key, (x, y)) in positions.iter() {
            draw_circle(*x, *y, r, color);
        }
    }

    fn draw_pieces(positions: &HashMap<String, (f32, f32)>, board_state: &mut HashMap<String, PlayerType>, p: f32) {
        let r = p / 4.;
        let mut color: Color;

        for (key, (x, y)) in positions.iter() {

            match board_state[key] {
                PlayerType::Player1 => color = WHITE,
                PlayerType::Player2 => color = BLACK,
                PlayerType::None => continue,
            }

            draw_circle(*x, *y, r, color);
        }
    }

    fn draw_lines(x_offset: f32, p: f32, color: Color) {
        let line_width: f32 = 6.0;
        let line_ext: f32 = line_width / 2.;

        let (mut x1, mut y1) = (0., p);
        let (mut x2, mut y2) = (p * 6., p);
    
        // Draw the 3 first horizontal lines
        for _ in 1..=3 {
            draw_line(x_offset - line_ext + x1, y1, x_offset + line_ext + x2, y2, line_width, color);
            (x1, y1) = (x1 + p, y1 + p);
            (x2, y2) = (x2 - p, y1);
        }
    
        // Draw the next 3 horizontal lines
        (x1, y1) = (p * 2., p * 5.);
        (x2, y2) = (p * 4., y1);
    
        for _ in 1..=3 {
            draw_line(x_offset + x1 - line_ext, y1, x_offset + line_ext + x2, y2, line_width, color);
            (x1, y1) = (x1 - p, y1 + p);
            (x2, y2) = (x2 + p, y1);
        }
    
        // Draw the 3 first vertical lines
        (x1, y1) = (0., p);
        (x2, y2) = (x1, y1 + p * 6.);
        for _ in 1..=3 {
            draw_line(x_offset + x1, y1, x_offset + x2, y2, line_width, color);
            (x1, y1) = (x1 + p, y1 + p);
            (x2, y2) = (x1, y2 - p);
        }
    
        // Draw the next 3 vertical lines
        (x1, y1) = (p * 4., p * 3.);
        (x2, y2) = (x1, p * 5.);
        for _ in 1..=3 {
            draw_line(x_offset + x1, y1, x_offset + x2, y2, line_width, color);
            (x1, y1) = (x1 + p, y1 - p);
            (x2, y2) = (x1, y2 + p);
        }
    
        // Draw the last 4 lines
        draw_line(x_offset + p * 3., p, x_offset + p * 3., p * 3., line_width, color); // top vertical
        draw_line(x_offset + p * 3., p * 5., x_offset + p * 3., p * 7., line_width, color); // bottom vertical
        draw_line(x_offset, p * 4., x_offset + p * 2., p * 4., line_width, color); // left horizontal
        draw_line(x_offset + p * 4., p * 4., x_offset + p * 6., p * 4., line_width, color); // right horizontal
    }
}

fn update_positions(p: f32, x_offset: f32) -> HashMap<String, (f32, f32)> {
    let mut positions: HashMap<String, (f32, f32)> = HashMap::new();

    let pos_a1 = (x_offset, p);
    let pos_a2 = (x_offset, p * 4.);
    let pos_a3 = (x_offset, p * 7.);

    let pos_b1 = (x_offset + p, p * 2.);
    let pos_b2 = (x_offset + p, p * 4.);
    let pos_b3 = (x_offset + p, p * 6.);

    let pos_c1 = (x_offset + p * 6., p);
    let pos_c2 = (x_offset + p * 6., p * 4.);
    let pos_c3 = (x_offset + p * 6., p * 7.);

    let pos_d1 = (x_offset + p * 5., p * 2.);
    let pos_d2 = (x_offset + p * 5., p * 4.);
    let pos_d3 = (x_offset + p * 5., p * 6.);

    let pos_e1 = (x_offset + p * 2., p * 3.);
    let pos_e2 = (x_offset + p * 2., p * 4.);
    let pos_e3 = (x_offset + p * 2., p * 5.);

    let pos_f1 = (x_offset + p * 4., p * 3.);
    let pos_f2 = (x_offset + p * 4., p * 4.);
    let pos_f3 = (x_offset + p * 4., p * 5.);

    let pos_g1 = (x_offset + p * 3., p * 1.);
    let pos_g2 = (x_offset + p * 3., p * 2.);
    let pos_g3 = (x_offset + p * 3., p * 3.);

    let pos_h1 = (x_offset + p * 3., p * 5.);
    let pos_h2 = (x_offset + p * 3., p * 6.);
    let pos_h3 = (x_offset + p * 3., p * 7.);

    positions.insert("a1".to_string(), pos_a1);
    positions.insert("a2".to_string(), pos_a2);
    positions.insert("a3".to_string(), pos_a3);

    positions.insert("b1".to_string(), pos_b1);
    positions.insert("b2".to_string(), pos_b2);
    positions.insert("b3".to_string(), pos_b3);

    positions.insert("c1".to_string(), pos_c1);
    positions.insert("c2".to_string(), pos_c2);
    positions.insert("c3".to_string(), pos_c3);

    positions.insert("d1".to_string(), pos_d1);
    positions.insert("d2".to_string(), pos_d2);
    positions.insert("d3".to_string(), pos_d3);

    positions.insert("e1".to_string(), pos_e1);
    positions.insert("e2".to_string(), pos_e2);
    positions.insert("e3".to_string(), pos_e3);

    positions.insert("f1".to_string(), pos_f1);
    positions.insert("f2".to_string(), pos_f2);
    positions.insert("f3".to_string(), pos_f3);

    positions.insert("g1".to_string(), pos_g1);
    positions.insert("g2".to_string(), pos_g2);
    positions.insert("g3".to_string(), pos_g3);

    positions.insert("h1".to_string(), pos_h1);
    positions.insert("h2".to_string(), pos_h2);
    positions.insert("h3".to_string(), pos_h3);

    return positions;
}

fn new_board_state(_p: f32, _x_offset: f32) -> HashMap<String, PlayerType> {
    let mut board_state: HashMap<String, PlayerType> = HashMap::new();

    board_state.insert("a1".to_string(), PlayerType::None);
    board_state.insert("a2".to_string(), PlayerType::None);
    board_state.insert("a3".to_string(), PlayerType::None);

    board_state.insert("b1".to_string(), PlayerType::None);
    board_state.insert("b2".to_string(), PlayerType::None);
    board_state.insert("b3".to_string(), PlayerType::None);

    board_state.insert("c1".to_string(), PlayerType::None);
    board_state.insert("c2".to_string(), PlayerType::None);
    board_state.insert("c3".to_string(), PlayerType::None);

    board_state.insert("d1".to_string(), PlayerType::None);
    board_state.insert("d2".to_string(), PlayerType::None);
    board_state.insert("d3".to_string(), PlayerType::None);

    board_state.insert("e1".to_string(), PlayerType::None);
    board_state.insert("e2".to_string(), PlayerType::None);
    board_state.insert("e3".to_string(), PlayerType::None);

    board_state.insert("f1".to_string(), PlayerType::None);
    board_state.insert("f2".to_string(), PlayerType::None);
    board_state.insert("f3".to_string(), PlayerType::None);

    board_state.insert("g1".to_string(), PlayerType::None);
    board_state.insert("g2".to_string(), PlayerType::None);
    board_state.insert("g3".to_string(), PlayerType::None);

    board_state.insert("h1".to_string(), PlayerType::None);
    board_state.insert("h2".to_string(), PlayerType::None);
    board_state.insert("h3".to_string(), PlayerType::None);

    return board_state;
}

fn define_hitboxes(positions: &HashMap<String, (f32, f32)>, p: f32) -> Vec<Circle> {
    let mut hitboxes = vec![];
    let r = p / 2.;
    for (_key, (x, y)) in positions.iter() {
        let hitbox = Circle::new(*x,*y,r);
        hitboxes.push(hitbox);
    }
    return hitboxes
}

fn place_piece(click_x: f32, click_y: f32, p: f32, positions: &HashMap<String, (f32, f32)>, board_state: &mut HashMap<String, PlayerType>, player_turn: PlayerType) -> bool {
    let mouse_circle = Circle::new(click_x, click_y, p / 9.);
    let r = p / 2.;

    let mut selected_pos: Option<String> = None;

    for (key, (x, y)) in positions.iter() {
        let hitbox = Circle::new(*x,*y,r);

        if mouse_circle.overlaps(&hitbox) {
            println!("You hit {}", key);
            selected_pos = Some(key.to_string())
        }
    }

    match selected_pos {
        Some(x) => { 
            board_state.insert(x, player_turn);
            return true; }
        None => return false,
    }
}

fn swap_player_turn(turn: PlayerType) -> PlayerType {
    match turn {
        PlayerType::Player1 => PlayerType::Player2,
        PlayerType::Player2 => PlayerType::Player1,
        PlayerType::None => PlayerType::None,
    }
}

fn window_conf() -> Conf {
    Conf {
        window_title: "Morrises' Men".to_owned(),
        fullscreen: false,
        window_resizable: true,
        window_width: 1080,
        window_height: 1080,
        ..Default::default()
    }
}

#[macroquad::main(window_conf)]
async fn main() {
    
    // set_fullscreen(true);
    
    let p: f32 = screen_height() / 8.0; // position unit
    let x_offset = screen_width() / 2. - ((p * 6.) / 2.);
    
    // Board state should be a dictionary of tuples: eg game_state["a1"] = ((x, y), Player1)
    let mut board_state = new_board_state(p, x_offset);
    let mut turn = PlayerType::Player1;

    loop {
        let p: f32 = screen_height() / 8.0; // position unit
        let x_offset = screen_width() / 2. - ((p * 6.) / 2.);
        let positions = update_positions(p, x_offset);
        
        clear_background(BEIGE);
        draw_board(p, x_offset, &positions ,&mut board_state);

        if is_mouse_button_pressed(MouseButton::Left) {
            let (click_x, click_y) = mouse_position();
            let change_turn = place_piece(click_x, click_y, p, &positions, &mut board_state, turn);
            if change_turn {
                turn = swap_player_turn(turn);
            }
        }
        next_frame().await;
    }
}
