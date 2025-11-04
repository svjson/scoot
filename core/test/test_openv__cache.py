from scoot_core import Connection, TableModel, ColumnModel, OperationEnv
from scoot_core.cache import Cache
from .table_model_fixture import INTEGER, VARCHAR

dummy_connection = Connection.__new__(Connection)


caramba__stuff_table_model = TableModel(
    "stuff",
    "carambadb",
    columns=[
        ColumnModel("Id", INTEGER, False, True, None),
        ColumnModel("StuffType", VARCHAR(50), True, False, None),
    ],
)

caramba__category_table_model = TableModel(
    "category",
    "carambadb",
    columns=[
        ColumnModel("Id", INTEGER, False, True, None),
        ColumnModel("CategoryName", VARCHAR(50), True, False, None),
    ],
)


test__stuff_table_model = TableModel(
    "stuff",
    "testdb",
    columns=[
        ColumnModel("Id", INTEGER, False, True, None),
        ColumnModel("StuffRating", INTEGER, False, True, None),
    ],
)


def test_openv_cache__put_and_get__same_keys__no_cache_supplied():
    # Given
    op_env = OperationEnv(dummy_connection)
    op_env.cache_table_model("carambadb", "stuff", caramba__stuff_table_model)

    # When
    cache_result = op_env.table_model_cache("carambadb", "stuff")

    # Then
    assert cache_result == caramba__stuff_table_model


def test_openv_cache__put_and_get__different_keys__no_cache_supplied():
    # Given
    op_env = OperationEnv(dummy_connection)
    op_env.cache_table_model("stuff", "carambadb", caramba__stuff_table_model)

    # When
    cache_result = op_env.table_model_cache("testdb", "stuff")

    # Then
    assert cache_result is None


def test_openv_cache__put_and_get__schema_separation__no_cache_supplied():
    # Given
    op_env = OperationEnv(dummy_connection)
    op_env.cache_table_model("carambadb", "stuff", caramba__stuff_table_model)
    op_env.cache_table_model("testdb", "stuff", test__stuff_table_model)

    # When
    caramba__cache_result = op_env.table_model_cache("carambadb", "stuff")
    testdb__cache_result = op_env.table_model_cache("testdb", "stuff")

    # Then
    assert caramba__cache_result == caramba__stuff_table_model
    assert caramba__cache_result != test__stuff_table_model
    assert testdb__cache_result == test__stuff_table_model
    assert testdb__cache_result != caramba__stuff_table_model


def test_openv_cache__put_and_get__same_schema__no_cache_supplied():
    # Given
    op_env = OperationEnv(dummy_connection)
    op_env.cache_table_model("carambadb", "stuff", caramba__stuff_table_model)
    op_env.cache_table_model("carambadb", "category", caramba__category_table_model)

    # When
    stuff_cache_result = op_env.table_model_cache("carambadb", "stuff")
    category_cache_result = op_env.table_model_cache("carambadb", "category")

    # Then
    assert stuff_cache_result == caramba__stuff_table_model
    assert stuff_cache_result != caramba__category_table_model
    assert category_cache_result == caramba__category_table_model
    assert category_cache_result != caramba__stuff_table_model


def test_openv_cache__put_and_get__same_schema__different_case__no_cache_supplied():
    # Given
    op_env = OperationEnv(dummy_connection)
    op_env.cache_table_model("carambadb", "stuFF", caramba__stuff_table_model)
    op_env.cache_table_model("carambadb", "category", caramba__category_table_model)

    # When
    stuff_cache_result = op_env.table_model_cache("carambadb", "stuff")
    category_cache_result = op_env.table_model_cache("carambadb", "Category")

    # Then
    assert stuff_cache_result == caramba__stuff_table_model
    assert stuff_cache_result != caramba__category_table_model
    assert category_cache_result == caramba__category_table_model
    assert category_cache_result != caramba__stuff_table_model


def test_openv_cache__get_cached_table_model__cache_supplied():
    # Given
    cache = {"carambadb": Cache()}
    cache["carambadb"].put_table_model("stuff", caramba__stuff_table_model)
    op_env = OperationEnv(dummy_connection, cache)

    # When
    cache_result = op_env.table_model_cache("carambadb", "stuff")

    # Then
    assert cache_result == caramba__stuff_table_model
    assert cache == op_env.cache


def test_openv_cache__put_and_get__different_keys__cache_supplied():
    # Given
    cache = {"carambadb": Cache()}
    cache["carambadb"].put_table_model("stuff", caramba__stuff_table_model)
    op_env = OperationEnv(dummy_connection, cache)

    # When
    cache_result = op_env.table_model_cache("testdb", "stuff")

    # Then
    assert cache_result is None
    assert cache == op_env.cache


def test_openv_cache__put_and_get__schema_separation__cache_supplied():
    # Given
    cache = {"carambadb": Cache()}
    cache["carambadb"].put_table_model("stuff", caramba__stuff_table_model)
    op_env = OperationEnv(dummy_connection, cache)
    op_env.cache_table_model("testdb", "stuff", test__stuff_table_model)

    # When
    caramba__cache_result = op_env.table_model_cache("carambadb", "stuff")
    testdb__cache_result = op_env.table_model_cache("testdb", "stuff")

    # Then
    assert caramba__cache_result == caramba__stuff_table_model
    assert caramba__cache_result != test__stuff_table_model
    assert testdb__cache_result == test__stuff_table_model
    assert testdb__cache_result != caramba__stuff_table_model
    assert cache == op_env.cache


def test_openv_cache__put_and_get__same_schema__cache_supplied():
    # Given
    cache = {}
    cache["carambadb"] = Cache()
    cache["carambadb"].put_table_model("stuff", caramba__stuff_table_model)
    cache["carambadb"].put_table_model("category", caramba__category_table_model)
    op_env = OperationEnv(dummy_connection, cache)

    # When
    stuff_cache_result = op_env.table_model_cache("carambadb", "stuff")
    category_cache_result = op_env.table_model_cache("carambadb", "category")

    # Then
    assert stuff_cache_result == caramba__stuff_table_model
    assert stuff_cache_result != caramba__category_table_model
    assert category_cache_result == caramba__category_table_model
    assert category_cache_result != caramba__stuff_table_model
    assert cache == op_env.cache


def test_openv_cache__put_and_get__same_schema__different_case__cache_supplied():
    # Given
    cache: dict = {}
    op_env = OperationEnv(dummy_connection, cache)
    assert hex(id(op_env.cache)) == hex(id(cache))
    op_env.cache_table_model("carambadb", "stuFF", caramba__stuff_table_model)
    op_env.cache_table_model("carambadb", "category", caramba__category_table_model)

    # When
    stuff_cache_result = op_env.table_model_cache("carambadb", "stuff")
    category_cache_result = op_env.table_model_cache("carambadb", "Category")

    # Then
    assert stuff_cache_result == caramba__stuff_table_model
    assert stuff_cache_result != caramba__category_table_model
    assert category_cache_result == caramba__category_table_model
    assert category_cache_result != caramba__stuff_table_model
    assert cache == op_env.cache
