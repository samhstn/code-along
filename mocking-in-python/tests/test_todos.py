from unittest.mock import Mock, patch
from nose.tools import assert_is_not_none

from services import get_todos


@patch('services.requests.get')
def test_request_response(mock_get):
    mock_get.return_value.ok = True
    response = get_todos()

    assert_is_not_none(response)
