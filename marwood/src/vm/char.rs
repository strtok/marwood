use std::fmt::Formatter;

const ALARM: u32 = 0x7;
const BACKSPACE: u32 = 0x8;
const DELETE: u32 = 0x7f;
const ESCAPE: u32 = 0x01b;
const NULL: u32 = 0x0;
const RETURN: u32 = 0xd;
const TAB: u32 = 0x9;

pub fn named_to_char(text: &str) -> Option<char> {
    match text {
        "alarm" => Some(char::from_u32(ALARM).unwrap()),
        "backspace" => Some(char::from_u32(BACKSPACE).unwrap()),
        "delete" => Some(char::from_u32(DELETE)).unwrap(),
        "escape" => Some(char::from_u32(ESCAPE)).unwrap(),
        "null" => Some(char::from_u32(NULL)).unwrap(),
        "return" => Some(char::from_u32(RETURN)).unwrap(),
        "tab" => Some(char::from_u32(TAB)).unwrap(),
        "space" => Some(' '),
        "newline" => Some('\n'),
        _ => None,
    }
}

pub fn write_escaped_char(c: char, f: &mut Formatter<'_>) -> std::fmt::Result {
    match c {
        ' ' => write!(f, "\\#space"),
        '\n' => write!(f, "\\#newline"),
        c => match c as u32 {
            ALARM => write!(f, "\\#alarm"),
            BACKSPACE => write!(f, "\\#backspace"),
            DELETE => write!(f, "\\#delete"),
            ESCAPE => write!(f, "\\#escape"),
            NULL => write!(f, "\\#null"),
            RETURN => write!(f, "\\#return"),
            TAB => write!(f, "\\#tab"),
            _ => write!(f, "\\#{}", c),
        },
    }
}
