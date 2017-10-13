cars = 100
space_in_a_car = 4.0 #if we use 4 for space_in_a_car, then carpool_capacity is an integer number, not a floating number.
drivers = 30
passengers = 90

cars_not_driven = cars - drivers #Result of operation can be stored in a variable.
cars_driven = drivers
carpool_capacity = cars_driven * space_in_a_car # operation with interger and floating number makes floating number
average_passengers_per_car = passengers / cars_driven # divide operator makes floating number.

print("There are", cars, "cars available.")
print("There are only", drivers, "drivers available.")
print("There will be", cars_not_driven, "empty cars today.")
print("We can transport", carpool_capacity, "people today.")
print("We have", passengers, "to carpool today.")
print("We need to put about", average_passengers_per_car, "in each car.")