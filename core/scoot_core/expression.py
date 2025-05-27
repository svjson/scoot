from sqlglot import exp

COMPARE_PREDICATES = {
    exp.EQ: "=",
    exp.NEQ: "!=",
    exp.GT: ">",
    exp.GTE: ">=",
    exp.LT: "<",
    exp.LTE: "<=",
}


def unwrap_cast(cast_expr: exp.Cast):
    return str(cast_expr.this.this)


def unwrap_paren(cast_expr: exp.Paren):
    return str(cast_expr.this.this)


def _flatten(conditional):
    if conditional.get("logical", None) == "OR":
        preds = {
            (cond.get("condition", {}).get("pred") or cond.get("logical"))
            for cond in conditional["conditions"]
            if "condition" in cond
        }
        lefthands = {
            cond["condition"]["lhs"]
            for cond in conditional["conditions"]
            if "condition" in cond
        }

        if preds == {"="} and len(lefthands) == 1:
            return {
                "columns": conditional.get("columns"),
                "condition": {
                    "lhs": lefthands.pop(),
                    "pred": "IN",
                    "rhs": list(
                        reversed(
                            [
                                cond["condition"]["rhs"]
                                for cond in conditional["conditions"]
                            ]
                        )
                    ),
                },
            }
        elif preds == {"IN", "="} and len(lefthands) == 1:
            rhs = []
            for cond in conditional["conditions"]:
                if isinstance(cond["condition"]["rhs"], list):
                    rhs += cond["condition"]["rhs"]
                else:
                    rhs.insert(0, (cond["condition"]["rhs"]))
            return {
                "columns": conditional.get("columns"),
                "condition": {"lhs": lefthands.pop(), "pred": "IN", "rhs": rhs},
            }

    return conditional


def parse_conditional(expr):
    if isinstance(expr, exp.Paren):
        return parse_conditional(expr.this)
    if isinstance(expr, exp.And):
        conditions = [parse_conditional(expr.left), parse_conditional(expr.right)]
        return {"columns": _extract_columns(conditions), "conditions": conditions}
    if isinstance(expr, exp.Or):
        conditions = [
            _flatten(parse_conditional(expr.left)),
            _flatten(parse_conditional(expr.right)),
        ]
        return _flatten(
            {
                "columns": _extract_columns(conditions),
                "logical": "OR",
                "conditions": conditions,
            }
        )

    return _parse_condition(expr)


def _extract_columns(conditions):
    columns = []

    for cond in conditions:
        for col in cond.get("columns", []):
            if col not in columns:
                columns.append(col)

    return columns


def _parse_condition(cond):
    pred_type = type(cond)
    if isinstance(cond, exp.EQ) and isinstance(cond.right, exp.Any):
        pred_type = exp.In

    condition = {}

    if pred_type == exp.In:
        col = cond.this.name
        values = []

        list_expr = cond.expressions
        if isinstance(cond, exp.EQ) and isinstance(cond.right, exp.Any):
            list_expr = cond.right.this.this.this.expressions

        for v in list_expr:
            if isinstance(v, exp.Introducer):
                values.append(v.expression.this)
            elif isinstance(v, exp.Literal):
                values.append(v.this)
            elif isinstance(v, exp.Cast):
                values.append(unwrap_cast(v))
            else:
                print(f"Unhandled expr type: {type(v)}")

        condition = {"lhs": col, "pred": "IN", "rhs": values}
    elif pred_type in COMPARE_PREDICATES.keys():
        lhs = cond.left.name
        pred = COMPARE_PREDICATES.get(type(cond))
        rhs = None
        if isinstance(cond.right, exp.Column):
            rhs = cond.right.name
        elif isinstance(cond.right, exp.Cast):
            rhs = unwrap_cast(cond.right)
        elif isinstance(cond.right, exp.Paren):
            rhs = unwrap_paren(cond.right)
        elif isinstance(cond.right, exp.Literal):
            rhs = cond.right.this
        else:
            rhs = cond.right.sql()

        condition = {"lhs": lhs, "pred": pred, "rhs": rhs}
    else:
        print(f"Unhandled check type: {type(cond)} - {cond}")

    return {"columns": [condition["lhs"]], "condition": condition}
