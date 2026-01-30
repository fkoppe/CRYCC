const tinyc = @import("std");

const Languages = enum {
    tinyc,
    own_language,
};

const ASTNode = union(Languages) {

};
