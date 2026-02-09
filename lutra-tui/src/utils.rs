use ratatui::layout::Rect;

pub fn clip_left(mut area: Rect, left: u16) -> Rect {
    let left = left.min(area.width);
    area.x += left;
    area.width -= left;
    area
}
pub fn clip_top(mut area: Rect, top: u16) -> Rect {
    let top = top.min(area.height);
    area.y += top;
    area.height -= top;
    area
}
