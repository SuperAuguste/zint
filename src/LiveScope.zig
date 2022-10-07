//! Function, block, other similar scopes

const tv = @import("types_and_values.zig");
const std = @import("std");
const Ast = std.zig.Ast;

const LiveScope = @This();

allocator: std.mem.Allocator,
parent: ?*LiveScope = null,
node_idx: Ast.Node.Index,
declarations: std.StringHashMapUnmanaged(tv.Declaration) = .{},
/// Resizes can modify element pointer locations, so we use a list of pointers
/// TODO: DODify this
children: std.ArrayListUnmanaged(*LiveScope) = .{},

pub const ScopeKind = enum { container, block, function };
pub fn scopeKind(scope: LiveScope, tree: Ast) ScopeKind {
    return switch (tree.nodes.items(.tag)[scope.node_idx]) {
        .container_decl,
        .container_decl_trailing,
        .container_decl_arg,
        .container_decl_arg_trailing,
        .container_decl_two,
        .container_decl_two_trailing,
        .tagged_union,
        .tagged_union_trailing,
        .tagged_union_two,
        .tagged_union_two_trailing,
        .tagged_union_enum_tag,
        .tagged_union_enum_tag_trailing,
        .root,
        .error_set_decl,
        => .container,
        else => .block,
    };
}

pub fn getLabel(scope: LiveScope, tree: Ast) ?Ast.TokenIndex {
    const token_tags = tree.tokens.items(.tag);

    return switch (scope.scopeKind(tree)) {
        .block => z: {
            const lbrace = tree.nodes.items(.main_token)[scope.node_idx];
            break :z if (token_tags[lbrace - 1] == .colon and token_tags[lbrace - 2] == .identifier)
                lbrace - 2
            else
                null;
        },
        else => null,
    };
}

pub const ParentScopeIterator = struct {
    maybe_scope: ?*LiveScope,

    pub fn next(psi: *ParentScopeIterator) ?*LiveScope {
        if (psi.maybe_scope) |scope| {
            const curr = scope;
            psi.maybe_scope = scope.parent;
            return curr;
        } else return null;
    }
};

pub fn parentScopeIterator(scope: *LiveScope) ParentScopeIterator {
    return ParentScopeIterator{ .maybe_scope = scope };
}
