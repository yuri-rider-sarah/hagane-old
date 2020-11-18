pub struct File<T> {
    text: Box<[T]> ,
    pos: usize,
}

impl<T> File<T> {
    pub fn new(text: Box<[T]>) -> File<T> {
        File { text, pos: 0 }
    }

    pub fn read(&mut self) -> Option<&T> {
        self.pos += 1;
        self.text.get(self.pos - 1)
    }

    pub fn step_back(&mut self) {
        self.pos -= 1;
    }
}

pub fn read_file_chars<P: AsRef<std::path::Path>>(path: P) -> std::io::Result<File<char>> {
    Ok(File {
        text: std::fs::read_to_string(path)?.chars().collect(),
        pos: 0,
    })
}
