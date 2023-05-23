open Lwt.Infix

type products = {
  id : string;
  title : string;
  description : string;
  price : float;
}

module type DB = Rapper_helper.CONNECTION

let new_uuid () = Uuidm.v `V4 |> Uuidm.to_string

let pool =
  Caqti_lwt.connect_pool ~max_size:10
    (Uri.of_string
       "postgresql://localhost/mvc?user=root&password=mysecretpassword")
  |> function
  | Ok v -> v
  | Error e ->
      Format.printf "%s\n" (Caqti_error.show e);
      exit 1

let make_query f =
  Caqti_lwt.Pool.use f pool >>= function
  | Ok v -> Lwt.return v
  | Error e -> Lwt.fail (Caqti_error.Exn e)

let migrate =
  [%rapper
    execute
      {sql| CREATE TABLE IF NOT EXISTS products (
        id uuid NOT NULL PRIMARY KEY,
        title TEXT,
        description TEXT,
        price FLOAT
      );
 |sql}]
    ()
  |> make_query

let () = Lwt_main.run migrate

let get_products () =
  [%rapper
    get_many
      {sql|
      SELECT @string{id}, @string{title}, @string{description}, @float{price} FROM products
 |sql}
      record_out]
    ()
  |> make_query

let get_single id =
  [%rapper
    get_one
      {sql|
      SELECT @string{id}, @string{title}, @string{description}, @float{price} FROM products
      where id = %string{id}
 |sql}
      record_out]
    ~id
  |> make_query

let insert_product product =
  [%rapper
    execute
      {sql|
      INSERT INTO products (id, title, description, price) VALUES (%string{id}, %string{title}, %string{description}, %float{price})
 |sql}
      record_in]
    product
  |> make_query
