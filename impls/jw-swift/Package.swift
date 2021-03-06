// swift-tools-version:5.2
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

/// Package for SwiftPM
public let package = Package(
    name: "jw-swift",
    products: [
        .executable(name: "step0_repl", targets: ["step0_repl"]),
        .executable(name: "step1_read_print", targets: ["step1_read_print"]),
        .executable(name: "mal", targets: ["mal"]),
        .library(name: "malLib", targets: ["malLib"])
    ],
    dependencies: [],
    targets: [
        .target(name: "step0_repl", dependencies: []),
        .target(name: "step1_read_print", dependencies: ["malLib"]),
        .target(name: "mal", dependencies: ["malLib"]),
        .target(name: "malLib", dependencies: []),
        .testTarget(name: "malTests", dependencies: ["malLib"])
    ]
)
