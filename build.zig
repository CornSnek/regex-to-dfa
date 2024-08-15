const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) !void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "test_regex_dfa_converter",
        .root_source_file = b.path("src/os_main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Test the regex to dfa converter app on the os");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/os_main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    const wasm_bin_str = "regex_dfa_converter";
    const webroot_str = "webroot";
    const wasm_lib = b.addExecutable(.{
        .name = wasm_bin_str,
        .root_source_file = b.path("src/wasm_main.zig"),
        .target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
        }),
        .optimize = optimize,
    });
    wasm_lib.entry = .disabled;
    wasm_lib.rdynamic = true;
    const wasm_step = b.step("wasm", "Build wasm binary");
    const install_website = b.addInstallDirectory(.{
        .source_dir = b.path(webroot_str),
        .install_dir = .bin,
        .install_subdir = webroot_str,
    });
    const install_wasm = b.addInstallArtifact(wasm_lib, .{
        .dest_sub_path = try std.fmt.allocPrint(b.allocator, "{s}/{s}.{s}", .{
            webroot_str,
            wasm_bin_str,
            "wasm",
        }),
    });
    wasm_step.dependOn(&install_wasm.step);
    install_wasm.step.dependOn(&install_website.step);
    install_website.step.dependOn(b.getUninstallStep()); //Reinstall website again
    const run_website_step = b.step("server", "Install wasm binary, website files, and run python http.server");
    const python_http = b.addSystemCommand(&.{ "python", "-m", "http.server", "-d", "zig-out/bin/webroot/" });
    run_website_step.dependOn(&python_http.step);
    python_http.step.dependOn(&install_wasm.step);
}
