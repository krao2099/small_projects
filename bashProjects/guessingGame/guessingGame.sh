#!/bin/bash
secret_number=$(shuf -i 1-100 -n1)

echo "Guess a number between 1 and 100 "

read input

while [ 1 ] 
do
    if [[ $input -eq $secret_number ]];
    then
        break
    elif [[ $input -gt $secret_number ]];
    then
        echo "To high, guess again"
    elif [[ $input -lt $secret_number ]];
    then
        echo "To low, guess again"
    fi
    read input
done

echo "You guessed right, the number was $secret_number"
    