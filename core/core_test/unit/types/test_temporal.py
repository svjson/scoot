from scoot_core import types


def test_temporal__initialization():
    # When
    temporal = types.Temporal(
        date=types.Date(min=(1, 1, 1), max=(9999, 12, 31), calendar="gregorian"),
        time=types.Time(clock="24", fsp=8, timezone=types.TimeZone(offset="t")),
    )

    # Then
    time = temporal.time
    assert isinstance(time, types.Time)
    assert time.clock == "24"
    assert time.fsp == 8


def test_time__clone():
    # Given
    time = types.Time(clock="24", fsp=8, timezone=types.TimeZone(offset="t"))

    # When
    clone = time.clone()

    # Then
    assert clone.clock == "24"
    assert clone.fsp == 8
    tz = clone.timezone
    assert tz is not None
    assert tz.offset == "t"
