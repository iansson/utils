use macroquad::prelude::*;

enum PlayerTurn {
    Player1,
    Player2,
}

fn draw_board(p: f32, x_offset: f32, positions: &Vec<(f32, f32)>) {
    let color: Color = BROWN;

    draw_lines(x_offset, p, color);
    draw_markers(positions, p, color);

    fn draw_markers(positions: &Vec<(f32, f32)>, p: f32, color: Color) {
        let r = p / 6.5;

        for (x, y) in positions.iter() {
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

fn define_positions(p: f32, x_offset: f32) -> Vec<(f32, f32)>{
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

    let positions = vec![pos_a1, pos_a2, pos_a3, pos_b1, pos_b2, pos_b3, pos_c1, pos_c2, pos_c3, pos_d1, pos_d2, pos_d3, pos_e1, pos_e2, pos_e3, pos_f1, pos_f2, pos_f3, pos_g1, pos_g2, pos_g3, pos_h1, pos_h2, pos_h3];
    return positions;
}

fn define_hitboxes(positions: &Vec<(f32, f32)>, p: f32) -> Vec<Circle> {
    let mut hitboxes = vec![];
    let r = p / 2.;
    for (x, y) in positions.iter() {
        let hitbox = Circle::new(*x,*y,r);
        hitboxes.push(hitbox);
    }
    return hitboxes
}

fn place_piece(click_x: f32, click_y: f32, p: f32, positions: &Vec<(f32, f32)>) {
    let mouse_circle = Circle::new(click_x, click_y, p / 3.);
    let hitboxes = define_hitboxes(positions, p);

    for circle in hitboxes.iter() {
        if mouse_circle.overlaps(&circle) {
            println!("You hit the thing!");
        }
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

    loop {

        let p: f32 = screen_height() / 8.0; // position unit
        let x_offset = screen_width() / 2. - ((p * 6.) / 2.);
        let positions = define_positions(p, x_offset);

        clear_background(BEIGE);
        draw_board(p, x_offset, &positions);

        if is_mouse_button_pressed(MouseButton::Left) {
            let (click_x, click_y) = mouse_position();
            place_piece(click_x, click_y, p, &positions);

        }

        next_frame().await
    }
}
