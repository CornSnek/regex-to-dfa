const std = @import("std");
pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const exe = b.addExecutable(.{
        .name = "test_regex_dfa_converter",
        .root_source_file = b.path("src/os_main.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(exe);
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Test the regex to dfa converter app on the os");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/os_main.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_exe_unit_tests.step);

    const wasm_bin_str = "regex_dfa_converter";
    const webroot_str = "webroot";
    const wasm_exe = b.addExecutable(.{
        .name = wasm_bin_str,
        .root_source_file = b.path("src/wasm_main.zig"),
        .target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
        }),
        .optimize = optimize,
    });
    wasm_exe.entry = .disabled;
    wasm_exe.rdynamic = true;
    wasm_exe.root_module.export_symbol_names = &.{
        "WasmAlloc",  "WasmFree",   "WasmFreeAll",   "WasmListAllocs",
        "RegexToDFA", "FlushPrint", "StatesStrings", "StatesStringsLen",
    };
    const wasm_step = b.step("wasm", "Build wasm binary");
    const install_website = b.addInstallDirectory(.{
        .source_dir = b.path(webroot_str),
        .install_dir = .bin,
        .install_subdir = webroot_str,
    });
    const install_wasm = b.addInstallArtifact(wasm_exe, .{
        .dest_sub_path = try std.fmt.allocPrint(b.allocator, "{s}/{s}.{s}", .{
            webroot_str,
            wasm_bin_str,
            "wasm",
        }),
    });
    wasm_step.dependOn(&install_wasm.step);
    install_wasm.step.dependOn(&install_website.step);
    install_website.step.dependOn(b.getUninstallStep()); //Reinstall website steps again
    const run_website_step = b.step("server", "Install wasm binary, website files, and run python http.server");
    const python_http = b.addSystemCommand(&.{ "python", "-m", "http.server", "-d", "zig-out/bin/webroot/" });
    run_website_step.dependOn(&python_http.step);
    python_http.step.dependOn(&install_wasm.step);
}
