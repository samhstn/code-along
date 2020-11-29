use server::Server;
use http::request::Request;

fn main() {
    let server = Server::new("127.0.0.1:8080".to_string());
    server.run();
}

mod server {
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
}

mod http {
    pub mod request {
        use super::method::Method;

        pub struct Request {
            path: String,
            query_string: Option<String>,
            method: Method
        }
    }

    mod method {
        pub enum Method {
            GET,
            POST,
            PUT,
            HEAD,
            CONNECT,
            OPTIONS,
            TRACE,
            PATCH
        }
    }
}
