open Model

let ( let* ) = Lwt.bind
let html_to_string html = Format.asprintf "%a" (Tyxml.Html.pp ()) html

let list_products products =
  let open Tyxml.Html in
  tr
    [
      th [ txt "id" ];
      th [ txt "title" ];
      th [ txt "description" ];
      th [ txt "price" ];
    ]
  :: List.map
       (fun p ->
         tr
           [
             td [ a ~a:[ a_href ("/" ^ p.id) ] [ txt p.id ] ];
             td [ txt p.title ];
             td [ txt p.description ];
             td [ txt (string_of_float p.price) ];
           ])
       products

let tables products =
  let open Tyxml.Html in
  div [ table (list_products products) ]

let template req products =
  let open Tyxml.Html in
  html
    (head (title (txt "Greeting")) [])
    (body
       [
         div
           [
             form
               ~a:[ a_method `Post; a_action "/create" ]
               [
                 input
                   ~a:
                     [
                       a_name "dream.csrf";
                       a_input_type `Hidden;
                       a_value (Dream.csrf_token req);
                     ]
                   ();
                 input
                   ~a:
                     [
                       a_name "title"; a_placeholder "Title"; a_input_type `Text;
                     ]
                   ();
                 input
                   ~a:
                     [
                       a_name "description";
                       a_placeholder "Description";
                       a_input_type `Text;
                     ]
                   ();
                 input
                   ~a:
                     [
                       a_name "price";
                       a_placeholder "Price";
                       a_input_type `Number;
                     ]
                   ();
                 button ~a:[ a_button_type `Submit ] [ txt "Criar" ];
               ];
           ];
         div [ tables products ];
       ])

let single_template product =
  let open Tyxml.Html in
  html
    (head (title (txt product.title)) [])
    (body
       [ div [ a ~a:[ a_href "/" ] [ txt "Go Back!" ]; tables [ product ] ] ])

let home_controller req =
  let* products = Model.get_products () in
  Dream.html (html_to_string (template req products))

let create_controller req =
  match%lwt Dream.form req with
  | `Ok [ ("description", description); ("price", price); ("title", title) ] ->
      let* () =
        insert_product
          {
            id = new_uuid ();
            title;
            description;
            price = float_of_string price;
          }
      in
      Dream.redirect req "/"
  | `Ok tl ->
      List.map (fun (a, b) -> Format.sprintf "%s->%s" a b) tl
      |> String.concat "|" |> Dream.html
  | _ -> Dream.html "Bad request!"

let single_controller req =
  let id = Dream.param req "id" in
  let* product = get_single id in
  Dream.html (html_to_string (single_template product))
