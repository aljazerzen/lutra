import lutra_bin
import generated

my_movie = generated.Movie(
    id=4, title="Blah", is_released=False, genre=generated.Genre(fiction=True)
)
my_movie2 = generated.Movie(
    id=4, title="Blah", is_released=False, genre=generated.Genre(other="action")
)

print(lutra_bin.encode(my_movie))
print(lutra_bin.decode(generated.Movie, lutra_bin.encode(my_movie)))

print(lutra_bin.encode(my_movie2))
print(lutra_bin.decode(generated.Movie, lutra_bin.encode(my_movie2)))
