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
    Remove, // After a player has completed a string, they remove a piece from the opponent
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Piece {
    color: PlayerType,
    unlocked: bool, // A locked piece may not be removed unless there are no other pieces available
}

struct Game {
    player1_phase: GamePhase,
    player1_score: u16,
    player1_placed: u16,
    player1_piece_total: u16,
    player2_phase: GamePhase,
    player2_score: u16,
    player2_placed: u16,
    player2_piece_total: u16,
    active_turn: PlayerType,
    board_state: HashMap<String, Option<Piece>>,
    holding_piece: Option<Piece>,
    holding_pos: String,

    scoring_vertical: Vec<Vec<String>>,
    scoring_horisontal: Vec<Vec<String>>,
}

impl Game {
    pub fn new() -> Game {
        let scoring_vertical: Vec<Vec<String>> = vec![
            vec!["a1".to_string(), "a2".to_string(), "a3".to_string()],
            vec!["b1".to_string(), "b2".to_string(), "b3".to_string()],
            vec!["c1".to_string(), "c2".to_string(), "c3".to_string()],
            vec!["d1".to_string(), "d2".to_string(), "d3".to_string()],
            vec!["e1".to_string(), "e2".to_string(), "e3".to_string()],
            vec!["f1".to_string(), "f2".to_string(), "f3".to_string()],
            vec!["g1".to_string(), "g2".to_string(), "g3".to_string()],
            vec!["h1".to_string(), "h2".to_string(), "h3".to_string()],
        ];

        let scoring_horisontal: Vec<Vec<String>> = vec![
            vec!["a1".to_string(), "g1".to_string(), "c1".to_string()],
            vec!["b1".to_string(), "g2".to_string(), "d1".to_string()],
            vec!["e1".to_string(), "g3".to_string(), "f1".to_string()],
            vec!["a2".to_string(), "b2".to_string(), "e2".to_string()],
            vec!["f2".to_string(), "d2".to_string(), "c2".to_string()],
            vec!["e3".to_string(), "h1".to_string(), "f3".to_string()],
            vec!["b3".to_string(), "h2".to_string(), "d3".to_string()],
            vec!["a3".to_string(), "h3".to_string(), "c3".to_string()],
        ];

        return Game {
            player1_phase: GamePhase::Setup,
            player2_phase: GamePhase::Setup,
            player1_score: 0,
            player2_score: 0,
            player1_placed: 0,
            player2_placed: 0,
            player1_piece_total: 9,
            player2_piece_total: 9,
            active_turn: PlayerType::Player1,
            board_state: new_board_state(),
            holding_piece: None,
            holding_pos: "".to_string(),
            scoring_vertical: scoring_vertical,
            scoring_horisontal: scoring_horisontal,
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
        draw_board(
            p,
            x_offset,
            positions,
            &mut self.board_state,
            &self.holding_pos,
        );

        // let score_str = format!("{} | {}", self.player1_score, self.player2_score);
        // draw_text(&score_str, p / 2., p / 2., p / 2., BLACK);

        fn draw_board(
            p: f32,
            x_offset: f32,
            positions: &HashMap<String, (f32, f32)>,
            board_state: &mut HashMap<String, Option<Piece>>,
            skip_pos: &String,
        ) {
            clear_background(BEIGE);
            let color: Color = BROWN;

            draw_lines(x_offset, p, color);
            draw_markers(positions, p, color);
            draw_pieces(positions, board_state, p, skip_pos);

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
                skip_pos: &String,
            ) {
                let r = p / 4.;
                let mut color: Color;

                for (key, (x, y)) in positions.iter() {
                    // Don't draw the skipped position
                    if key == skip_pos {
                        continue;
                    }

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

    pub fn get_piece(&self, pos: String) -> Option<Piece> {
        let piece = self.board_state[&pos];
        return piece;
    }

    pub fn update_score(&mut self, pos: String) -> bool {
        // Check horisontal lines
        let mut scored = self.get_score(pos.to_string(), &self.scoring_horisontal);
        // Check vertical lines
        scored = scored || self.get_score(pos.to_string(), &self.scoring_vertical);

        // Give the score to the right player
        // match self.active_turn {
        //     PlayerType::Player1 => self.player1_score += score,
        //     PlayerType::Player2 => self.player2_score += score,
        // }

        return scored;
    }

    fn get_score(&self, pos: String, scoring_vector: &Vec<Vec<String>>) -> bool {
        let mut res = false;
        for line in scoring_vector.iter() {
            if !line.contains(&pos) {
                continue;
            }

            // update score if line is filled by active player
            let mut scored = true;
            for scoring_pos in line {
                match self.get_piece(scoring_pos.to_string()) {
                    None => {
                        scored = false;
                        break;
                    }
                    Some(piece) => {
                        if piece.color != self.active_turn {
                            scored = false;
                            break;
                        }
                    }
                }
            }

            if scored {
                res = true;
                for scoring_pos in line {
                    let mut piece = self.get_piece(scoring_pos.to_string()).unwrap();
                    piece.unlocked = false;
                }
            }
        }
        return res;
    }

    fn set_active_phase(&mut self, phase: GamePhase) {
        match self.active_turn {
            PlayerType::Player1 => self.player1_phase = phase,
            PlayerType::Player2 => self.player2_phase = phase,
        }
    }

    fn reset_hand_piece(&mut self) {
        self.holding_piece = None;
        self.holding_pos = "".to_string();
    }

    fn increment_placed(&mut self) -> u16 {
        match self.active_turn {
            PlayerType::Player1 => {
                self.player1_placed += 1;
                return self.player1_placed;
            }
            PlayerType::Player2 => {
                self.player2_placed += 1;
                return self.player2_placed;
            }
        }
    }

    pub fn next_action(&mut self, position: Option<String>) {
        match self.active_game_state() {
            GamePhase::Setup => self.place_piece(position),
            GamePhase::Moving => self.move_piece(position),
            GamePhase::Flying => self.move_piece(position),
            GamePhase::Remove => self.remove_piece(position),
        }
    }

    fn get_valid_moves(&self) -> Vec<String> {
        // the resulting vector does not include the position in question
        // NOTE: to = position and from = self.holding_pos

        let mut res = vec![];

        // Get valid horisontal moves
        for line in self.scoring_horisontal.iter() {
            if !line.contains(&self.holding_pos) {
                continue;
            }
            res = _get_valid_move(line, &res, &self.holding_pos);
            break;
        }
        // Get valid vertical moves
        for line in self.scoring_vertical.iter() {
            if !line.contains(&self.holding_pos) {
                continue;
            }
            res = _get_valid_move(line, &res, &self.holding_pos);
            break;
        }

        return res;

        fn _get_valid_move(line: &Vec<String>, res: &Vec<String>, pos: &String) -> Vec<String> {
            let mut res = res.to_vec();

            // Find position
            let mut relative_pos: usize = 42;
            for (i, i_pos) in line.iter().enumerate() {
                if i_pos == pos {
                    relative_pos = i;
                }
            }

            match relative_pos {
                0 => res.push(line[1].to_string()),
                1 => {
                    res.push(line[0].to_string());
                    res.push(line[2].to_string());
                }
                2 => res.push(line[1].to_string()),
                _ => panic!("Error: Faulty relative position '{}'", relative_pos),
            }

            return res;
        }
    }

    pub fn remove_piece(&mut self, position: Option<String>) {
        match position {
            // Check if piece selection was successful
            None => return,
            Some(pos) => {
                match self.board_state[&pos] {
                    None => return,
                    Some(_) => {
                        let unlocked_positions = self.get_unlocked_positions(self.opponent_color());

                        // If requirements met, remove the piece
                        if unlocked_positions.len() == 0 || unlocked_positions.contains(&pos) {
                            // Update piece total counter
                            match self.active_turn {
                                PlayerType::Player1 => self.player2_piece_total -= 1,
                                PlayerType::Player2 => self.player1_piece_total -= 1,
                            }
                            // Remove the piece from the board
                            self.board_state.insert(pos, None);
                            // Go back to the previous state
                            self.update_phase(self.active_turn);

                            // Swap turn
                            self.swap_turn();
                        }
                    }
                }
            }
        }
    }

    fn update_phase(&mut self, color: PlayerType) {
        match color {
            PlayerType::Player1 => {
                self.player1_phase = if self.player1_placed < 9 {
                    GamePhase::Setup
                } else if self.player1_piece_total <= 3 {
                    GamePhase::Flying
                } else {
                    GamePhase::Moving
                }
            }
            PlayerType::Player2 => {
                self.player2_phase = if self.player2_placed < 9 {
                    GamePhase::Setup
                } else if self.player2_piece_total <= 3 {
                    GamePhase::Flying
                } else {
                    GamePhase::Moving
                }
            }
        }
    }

    fn get_unlocked_positions(&self, color: PlayerType) -> Vec<String> {
        let mut res = vec![];

        for (key, piece) in self.board_state.iter() {
            match piece {
                None => continue,
                Some(piece) => {
                    if piece.color == color && piece.unlocked {
                        res.push(key.to_string());
                    }
                }
            }
        }

        return res;
    }

    pub fn move_piece(&mut self, position: Option<String>) {
        // Check if we are already holding a piece
        match self.holding_piece {
            None => self.take_piece(position),
            Some(piece) => self.move_piece_from_hand(position, piece),
        }
    }

    fn take_piece(&mut self, position: Option<String>) {
        match position {
            // Player didn't click on a valid marker
            None => return,
            // Player did click on a valid marker
            Some(pos) => match self.board_state[&pos] {
                Some(piece) => {
                    if piece.color == self.active_turn {
                        self.holding_piece = Some(piece);
                        self.holding_pos = pos;
                    }
                }
                _ => return,
            },
        }
    }

    fn move_piece_from_hand(&mut self, position: Option<String>, piece: Piece) {
        match position {
            // Player didn't click on a valid marker
            None => return,
            // Player did click on a valid marker
            Some(pos) => match self.board_state[&pos] {
                // The position has no piece
                None => {
                    // Verify position
                    if self.active_game_state() == GamePhase::Moving {
                        let valid_moves = self.get_valid_moves();
                        if !valid_moves.contains(&pos) {
                            println!("Invalid move!");
                            return;
                        }
                    }

                    // Place the piece
                    self.board_state.insert(
                        pos.to_string(),
                        Some(Piece {
                            color: self.active_turn,
                            unlocked: true,
                        }),
                    );
                    // Remove piece from board
                    self.board_state.insert(self.holding_pos.clone(), None);
                    // Remove piece from hand
                    self.reset_hand_piece();

                    // Update score
                    let scored = self.update_score(pos.to_string());

                    if scored {
                        self.set_active_phase(GamePhase::Remove);
                    } else {
                        // Change turn
                        self.swap_turn();
                    }
                }
                // The position has a piece
                Some(_) => {
                    // If the position selected is the same as the holding piece, put it back
                    if pos == self.holding_pos {
                        // Remove piece from hand
                        self.reset_hand_piece();
                    }
                }
            },
        }
    }

    pub fn place_piece(&mut self, position: Option<String>) {
        match position {
            // Check if piece selection was successful
            None => return,
            Some(pos) => match self.board_state[&pos] {
                // Check if there already is a piece at selected position
                Some(_) => return,
                None => {
                    // Logic for placing the piece
                    self.board_state.insert(
                        pos.to_string(),
                        Some(Piece {
                            color: self.active_turn,
                            unlocked: true,
                        }),
                    );

                    if self.active_game_state() == GamePhase::Setup {
                        // Update placed count
                        let placed_count = self.increment_placed();
                        // Change to 'Moving' phase if all pieces have been placed
                        // if placed_count >= 9 {
                        //     self.set_active_phase(GamePhase::Moving);
                        // }
                        self.update_phase(self.active_turn);
                    }

                    // Update score
                    let scored = self.update_score(pos.to_string());

                    if scored {
                        self.set_active_phase(GamePhase::Remove);
                    } else {
                        // Change turn
                        self.swap_turn();
                    }
                }
            },
        }
    }

    pub fn active_game_state(&self) -> GamePhase {
        match self.active_turn {
            PlayerType::Player1 => return self.player1_phase.clone(),
            PlayerType::Player2 => return self.player2_phase.clone(),
        }
    }

    pub fn opponent_color(&self) -> PlayerType {
        match self.active_turn {
            PlayerType::Player1 => return PlayerType::Player2,
            PlayerType::Player2 => return PlayerType::Player1,
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
            game.next_action(selected);
        }
        next_frame().await;
    }
}
