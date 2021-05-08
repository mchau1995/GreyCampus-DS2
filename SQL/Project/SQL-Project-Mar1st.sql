SELECT staff.first_name as manager_first_name, staff.last_name as manager_last_name, 
address.address, address.district, city.city, country.country from staff
inner join address on
staff.address_id = address.address_id
inner join city on
address.city_id = city.city_id
inner join country on
city.country_id = country.country_id;

SELECT inventory.inventory_id, inventory.store_id, 
film.title, film.rating, film.rental_rate, film.replacement_cost from inventory
inner join film on inventory.film_id = film.film_id;

SELECT inventory.store_id, film. rating, count(film.rating) as inventory_items from film 
inner join inventory on inventory.film_id = film.film_id
where film.rating in (SELECT film.rating from inventory
inner join film on inventory.film_id = film.film_id)
group by film.rating, inventory.store_id
order by store_id asc;

SELECT inventory.store_id, category.name, 
count(category.name) as films,
avg(film.replacement_cost) as avg_replacement_cost, 
sum(film.replacement_cost) as total_replacement_cost from category 
inner join film_category on film_category.category_id = category.category_id
inner join film on film.film_id = film_category.film_id
inner join inventory on inventory.film_id = film.film_id
group by inventory.store_id, category.name
order by total_replacement_cost desc;


select customer.first_name, customer.last_name, customer.store_id, customer.active, address.address,
city.city, country.country from customer 
inner join address on customer.address_id = address.address_id
inner join city on address.city_id = city.city_id
inner join country on city.country_id = country.country_id;

SELECT customer.first_name, customer.last_name, count(rental.rental_id) as total_rentals,
sum(payment.amount) as total_payment_amount
from customer
inner join payment on payment.customer_id = customer.customer_id
inner join rental on rental.rental_id = payment.rental_id
group by customer.first_name, customer.last_name
order by total_payment_amount desc;


