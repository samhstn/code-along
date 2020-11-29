pub struct Server {
    addr: String,
}

impl Server {
    pub fn new(addr: String) -> Self { // can also be Server instead of Self
        Self { addr }
    }

    pub fn run(self) {
        println!("Listening on {}", self.addr);
    }
}
