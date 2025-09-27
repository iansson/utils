use macroquad::prelude::*;
use std::collections::HashMap;

#[derive(Clone, Copy, PartialEq, Eq)]
enum PlayerType {
    Player1,
    Player2,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum GamePhase {
    Setup,  // Each player places 9 pieces
    Moving, // Each player moves their own pieces
    Flying, // If a player only has 3 pieces, they may move their pieces without restriction
}

struct Piece {
    color: PlayerType,
    movable: bool,
}

struct Game {
    player1_phase: GamePhase,
    player1_score: u16,
    player1_placed: u16,
    player2_phase: GamePhase,
    player2_score: u16,
    player2_placed: u16,
    active_turn: PlayerType,
    board_state: HashMap<String, Option<Piece>>,
}

impl Game {
    pub fn new() -> Game {
        return Game {
            player1_phase: GamePhase::Setup,
            player2_phase: GamePhase::Setup,
            player1_score: 0,
            player2_score: 0,
            player1_placed: 0,
            player2_placed: 0,
            active_turn: PlayerType::Player1,
            board_state: new_board_state(),
        };

        fn new_board_state() -> HashMap<String, Option<Piece>> {
            let mut board_state: HashMap<String, Option<Piece>> = HashMap::new();

            board_state.insert("a1".to_string(), None);
            board_state.insert("a2".to_string(), None);
            board_state.insert("a3".to_string(), None);
            board_state.insert("b1".to_string(), None);
            board_state.insert("b2".to_string(), None);
            board_state.insert("b3".to_string(), None);
            board_state.insert("c1".to_string(), None);
            board_state.insert("c2".to_string(), None);
            board_state.insert("c3".to_string(), None);
            board_state.insert("d1".to_string(), None);
            board_state.insert("d2".to_string(), None);
            board_state.insert("d3".to_string(), None);
            board_state.insert("e1".to_string(), None);
            board_state.insert("e2".to_string(), None);
            board_state.insert("e3".to_string(), None);
            board_state.insert("f1".to_string(), None);
            board_state.insert("f2".to_string(), None);
            board_state.insert("f3".to_string(), None);
            board_state.insert("g1".to_string(), None);
            board_state.insert("g2".to_string(), None);
            board_state.insert("g3".to_string(), None);
            board_state.insert("h1".to_string(), None);
            board_state.insert("h2".to_string(), None);
            board_state.insert("h3".to_string(), None);

            return board_state;
        }
    }

    pub fn draw(&mut self, p: f32, x_offset: f32, positions: &HashMap<String, (f32, f32)>) {
        draw_board(p, x_offset, positions, &mut self.board_state);

        fn draw_board(
            p: f32,
            x_offset: f32,
            positions: &HashMap<String, (f32, f32)>,
            board_state: &mut HashMap<String, Option<Piece>>,
        ) {
            clear_background(BEIGE);
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

            fn draw_pieces(
                positions: &HashMap<String, (f32, f32)>,
                board_state: &mut HashMap<String, Option<Piece>>,
                p: f32,
            ) {
                let r = p / 4.;
                let mut color: Color;

                for (key, (x, y)) in positions.iter() {
                    // match board_state[key] {
                    // PlayerType::Player1 => color = WHITE,
                    // PlayerType::Player2 => color = BLACK,
                    // PlayerType::None => continue,
                    // }

                    match &board_state[key] {
                        Some(piece) => match piece.color {
                            PlayerType::Player1 => color = WHITE,
                            PlayerType::Player2 => color = BLACK,
                        },
                        None => continue,
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
                    draw_line(
                        x_offset - line_ext + x1,
                        y1,
                        x_offset + line_ext + x2,
                        y2,
                        line_width,
                        color,
                    );
                    (x1, y1) = (x1 + p, y1 + p);
                    (x2, y2) = (x2 - p, y1);
                }

                // Draw the next 3 horizontal lines
                (x1, y1) = (p * 2., p * 5.);
                (x2, y2) = (p * 4., y1);

                for _ in 1..=3 {
                    draw_line(
                        x_offset + x1 - line_ext,
                        y1,
                        x_offset + line_ext + x2,
                        y2,
                        line_width,
                        color,
                    );
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
                draw_line(
                    x_offset + p * 3.,
                    p,
                    x_offset + p * 3.,
                    p * 3.,
                    line_width,
                    color,
                ); // top vertical
                draw_line(
                    x_offset + p * 3.,
                    p * 5.,
                    x_offset + p * 3.,
                    p * 7.,
                    line_width,
                    color,
                ); // bottom vertical
                draw_line(
                    x_offset,
                    p * 4.,
                    x_offset + p * 2.,
                    p * 4.,
                    line_width,
                    color,
                ); // left horizontal
                draw_line(
                    x_offset + p * 4.,
                    p * 4.,
                    x_offset + p * 6.,
                    p * 4.,
                    line_width,
                    color,
                ); // right horizontal
            }
        }
    }

    pub fn swap_turn(&mut self) {
        match self.active_turn {
            PlayerType::Player1 => self.active_turn = PlayerType::Player2,
            PlayerType::Player2 => self.active_turn = PlayerType::Player1,
        }
    }

    pub fn select_position(
        &mut self,
        click_x: f32,
        click_y: f32,
        p: f32,
        positions: &HashMap<String, (f32, f32)>,
    ) -> Option<String> {
        let mouse_circle = Circle::new(click_x, click_y, p / 9.);
        let r = p / 2.;

        let mut selected_pos: Option<String> = None;

        for (key, (x, y)) in positions.iter() {
            let hitbox = Circle::new(*x, *y, r);

            if mouse_circle.overlaps(&hitbox) {
                selected_pos = Some(key.to_string());
                return selected_pos;
            }
        }
        return selected_pos;
    }

    pub fn place_piece(&mut self, position: Option<String>) {
        match position {
            Some(x) => match self.board_state[&x] {
                Some(_) => return,
                None => {
                    self.board_state.insert(
                        x,
                        Some(Piece {
                            color: self.active_turn,
                            movable: true,
                        }),
                    );
                    self.swap_turn();
                }
            },
            None => return,
        }
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

fn window_conf() -> Conf {
    Conf {
        window_title: "Nine Men's Morris".to_owned(),
        fullscreen: false,
        window_resizable: true,
        window_width: 1080,
        window_height: 1080,
        ..Default::default()
    }
}

// we could keep a list over nodes that are part of a won line
// that way we could more easily decide if we are allowed to move a placed piece
// **OR we just mark pieces as unmovable**

#[macroquad::main(window_conf)]
async fn main() {
    // set_fullscreen(true);

    // let p: f32 = screen_height() / 8.0; // position unit
    // let x_offset = screen_width() / 2. - ((p * 6.) / 2.);

    // Board state should be a dictionary of tuples: eg game_state["a1"] = ((x, y), Player1)
    let mut game = Game::new();

    loop {
        let p: f32 = screen_height() / 8.0; // position unit
        let x_offset = screen_width() / 2. - ((p * 6.) / 2.);
        let positions = update_positions(p, x_offset);

        game.draw(p, x_offset, &positions);

        if is_mouse_button_pressed(MouseButton::Left) {
            let (click_x, click_y) = mouse_position();
            let selected = game.select_position(click_x, click_y, p, &positions);
            game.place_piece(selected);
        }
        next_frame().await;
    }
}
