#![allow(dead_code)]

use server::Server;
use website_handler::WebsiteHandler;

mod server;
mod http;
mod website_handler;

fn main() {
    let server = Server::new("127.0.0.1:8080".to_string());
    // server.run(WebsiteHandler::new(public_path));
    server.run(WebsiteHandler);
}
