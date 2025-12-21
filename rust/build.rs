fn main() {
    // Link against Accelerate framework on macOS
    #[cfg(target_os = "macos")]
    {
        println!("cargo:rustc-link-lib=framework=Accelerate");
    }
}
