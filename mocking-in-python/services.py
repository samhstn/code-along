from urllib.parse import urljoin

import requests

TODOS_URL = 'http://jsonplaceholder.typicode.com/todos'


def get_todos():
    response = requests.get(TODOS_URL)

    if response.ok:
        return response
    else:
        return None
