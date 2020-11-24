fn main() {
    println!("Weight on Mars: {}kg", calculate_weight_on_mars(100.0));
}

fn calculate_weight_on_mars(weight: f32) -> f32 {
    (weight / 9.81) * 3.711
}
