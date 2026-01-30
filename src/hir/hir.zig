const frontend = @import("../frontend/frontend.zig");

const HIRData = struct {
    language_data: HIRLanguageData,
};

const HIRLanguageData = union(frontend.Languages) {

};
