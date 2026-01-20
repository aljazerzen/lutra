import unittest

import lutra_runner_postgres

# generated
from . import lutra_generated as l


class TestCase(unittest.IsolatedAsyncioTestCase):
    async def test_x(self) -> None:
        pg = await lutra_runner_postgres.Runner.connect(
            "postgres://postgres:pass@localhost:5416"
        )

        movie1 = l.Movie(id=2, title="Forrest Gump", is_released=True)
        movie2 = l.Movie(id=9, title="Prestige", is_released=False)

        await pg.run(l.insert_movie(), movie1)
        await pg.run(l.insert_movie(), movie2)

        movies = await pg.run(l.from_movies(), ())

        self.assertEqual(movie1, movies[0])
        self.assertEqual(movie2, movies[1])

        await pg.shutdown()
