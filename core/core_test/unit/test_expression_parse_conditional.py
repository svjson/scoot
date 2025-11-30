from scoot_core.expression import parse_conditional

import sqlglot


def test_parse_conditional__mssql__gte_zero_constraint():
    # Given
    sqltext = '("total_amount" >= (0))'
    expr = sqlglot.parse_one(sqltext, read="tsql")

    # When
    result = parse_conditional(expr)

    # Then
    assert result == {
        "columns": ["total_amount"],
        "condition": {"lhs": "total_amount", "pred": ">=", "rhs": "0"},
    }


def test_parse_conditional__mssql__in_string_list():
    # Given
    sqltext = """("status" = 'cancelled' OR "status" = 'delivered' OR "status" = 'shipped' OR "status" = 'pending')"""
    expr = sqlglot.parse_one(sqltext, read="tsql")

    # When
    result = parse_conditional(expr)

    # Then
    assert result == {
        "columns": ["status"],
        "condition": {
            "lhs": "status",
            "pred": "IN",
            "rhs": ["pending", "shipped", "delivered", "cancelled"],
        },
    }


def test_parse_conditional__mysql__gte_zero_constraint():
    # Given
    sqltext = "(`total_amount` >= 0)"
    expr = sqlglot.parse_one(sqltext, read="mysql")

    # When
    result = parse_conditional(expr)

    # Then
    assert result == {
        "columns": ["total_amount"],
        "condition": {"lhs": "total_amount", "pred": ">=", "rhs": "0"},
    }


def test_parse_conditional__mysql__in_string_list():
    # Given
    sqltext = "(`status` in (_utf8mb4'pending',_utf8mb4'shipped',_utf8mb4'delivered',_utf8mb4'cancelled'))"
    expr = sqlglot.parse_one(sqltext, read="mysql")

    # When
    result = parse_conditional(expr)

    # Then
    assert result == {
        "columns": ["status"],
        "condition": {
            "lhs": "status",
            "pred": "IN",
            "rhs": ["pending", "shipped", "delivered", "cancelled"],
        },
    }


def test_parse_conditional__postgres__gte_zero_constraint():
    # Given
    sqltext = "total_amount >= CAST(0 AS DECIMAL)"
    expr = sqlglot.parse_one(sqltext, read="postgres")

    # When
    result = parse_conditional(expr)

    # Then
    assert result == {
        "columns": ["total_amount"],
        "condition": {"lhs": "total_amount", "pred": ">=", "rhs": "0"},
    }


def test_parse_conditional__postgres__in_string_list():
    # Given
    sqltext = "CAST(status AS TEXT) = ANY (CAST(ARRAY(CAST('pending' AS VARCHAR), CAST('shipped' AS VARCHAR), CAST('delivered' AS VARCHAR), CAST('cancelled' AS VARCHAR)) AS ARRAY<TEXT>))"
    expr = sqlglot.parse_one(sqltext, read="postgres")

    # When
    result = parse_conditional(expr)

    # Then
    assert result == {
        "columns": ["status"],
        "condition": {
            "lhs": "status",
            "pred": "IN",
            "rhs": ["pending", "shipped", "delivered", "cancelled"],
        },
    }


def test_parse_conditional__oracle__gte_zero_constraint():
    # Given
    sqltext = "total_amount >= 0"
    expr = sqlglot.parse_one(sqltext, read="oracle")

    # When
    result = parse_conditional(expr)

    # Then
    assert result == {
        "columns": ["total_amount"],
        "condition": {"lhs": "total_amount", "pred": ">=", "rhs": "0"},
    }


def test_parse_conditional__oracle__in_string_list():
    # Given
    sqltext = "status IN ('pending', 'shipped', 'delivered', 'cancelled')"
    expr = sqlglot.parse_one(sqltext, read="oracle")

    # When
    result = parse_conditional(expr)

    # Then
    assert result == {
        "columns": ["status"],
        "condition": {
            "lhs": "status",
            "pred": "IN",
            "rhs": ["pending", "shipped", "delivered", "cancelled"],
        },
    }
