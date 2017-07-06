% PART 1 

%This function calculate the length of each consecutive numbers
helper1([],[]).
helper1([Head], [[Head,1]]).
helper1([Head|Tail], [[Head, R] | Rest]):- 
    helper1(Tail, [[Head,RS] | Rest]), succ(RS, R), !.
helper1([Head|Tail], [[Head, 1], [X, RS] | Rest]):-
    helper1(Tail, [[X, RS] | Rest]), Head \=X, !.


%return empty if empty
helper2([], []).
%If 1 is 1-2, add '.'
helper2([[1,1]|T], ['.' | R]):- helper2(T, R).
helper2([[1,2]|T], ['.' | R]):- helper2(T, R).
%If 1 is greater or equal to 2, add '-'
helper2([[1,X]|T], ['-' | R]):- X >= 2, helper2(T, R).
% If 0 is 1-2, we don't need to add anything to the output
helper2([[0,1]|T], R):- helper2(T, R).
helper2([[0,2]|T], R):- helper2(T, R).
% If 0 is 2 < x <= 5, add '^' to the output %
helper2([[0,2]|T], ['^' | R]):- helper2(T, R).
helper2([[0,3]|T], ['^' | R]):- helper2(T, R).
helper2([[0,4]|T], ['^' | R]):- helper2(T, R).
helper2([[0,5]|T], ['^' | R]):- helper2(T, R).
% If 0 is greater or eaqual to 5, add '#'
helper2([[0,X]|T], ['#' | R]):- X >= 5, helper2(T, R).


signal_morse([],[]).
signal_morse([Head | Tail], R1):- helper1([Head | Tail], R2), helper2(R2,R1).


%PART 2

morse(a, [.,-]).           % A
    morse(b, [-,.,.,.]).   % B
    morse(c, [-,.,-,.]).   % C
    morse(d, [-,.,.]).   % D
    morse(e, [.]).   % E
morse('e''', [.,.,-,.,.]). % É (accented E)
    morse(f, [.,.,-,.]).   % F
    morse(g, [-,-,.]).   % G
    morse(h, [.,.,.,.]).   % H
    morse(i, [.,.]).   % I
    morse(j, [.,-,-,-]).   % J
    morse(k, [-,.,-]).   % K or invitation to transmit
    morse(l, [.,-,.,.]).   % L
    morse(m, [-,-]).   % M
    morse(n, [-,.]).   % N
    morse(o, [-,-,-]).   % O
    morse(p, [.,-,-,.]).   % P
    morse(q, [-,-,.,-]).   % Q
    morse(r, [.,-,.]).   % R
    morse(s, [.,.,.]).   % S
    morse(t, [-]).    % T
    morse(u, [.,.,-]).   % U
    morse(v, [.,.,.,-]).   % V
    morse(w, [.,-,-]).   % W
    morse(x, [-,.,.,-]).   % X or multiplication sign
    morse(y, [-,.,-,-]).   % Y
    morse(z, [-,-,.,.]).   % Z
    morse(0, [-,-,-,-,-]).   % 0
    morse(1, [.,-,-,-,-]).   % 1
    morse(2, [.,.,-,-,-]).   % 2
    morse(3, [.,.,.,-,-]).   % 3
    morse(4, [.,.,.,.,-]).   % 4
    morse(5, [.,.,.,.,.]).   % 5
    morse(6, [-,.,.,.,.]).   % 6
    morse(7, [-,-,.,.,.]).   % 7
    morse(8, [-,-,-,.,.]).   % 8
    morse(9, [-,-,-,-,.]).   % 9
morse(., [.,-,.,-,.,-]).   % . (period)
morse(',', [-,-,.,.,-,-]). % , (comma)
morse(:, [-,-,-,.,.,.]).   % : (colon or division sign)
morse(?, [.,.,-,-,.,.]).   % ? (question mark)
morse('''',[.,-,-,-,-,.]). % ' (apostrophe)
morse(-, [-,.,.,.,.,-]).   % - (hyphen or dash or subtraction sign)
morse(/, [-,.,.,-,.]).     % / (fraction bar or division sign)
morse('(', [-,.,-,-,.]).   % ( (left-hand bracket or parenthesis)
morse(')', [-,.,-,-,.,-]). % ) (right-hand bracket or parenthesis)
morse('"', [.,-,.,.,-,.]). % " (inverted commas or quotation marks)
morse(=, [-,.,.,.,-]).     % = (double hyphen)
morse(+, [.,-,.,-,.]).     % + (cross or addition sign)
morse(@, [.,-,-,.,-,.]).   % @ (commercial at)

% Error.
morse(error, [.,.,.,.,.,.,.,.]). % error - see below

% Prosigns.
    morse(as, [.,-,.,.,.]).      % AS (wait A Second)
morse(ct, [-,.,-,.,-]).          % CT (starting signal, Copy This)
morse(sk, [.,.,.,-,.,-]).        % SK (end of work, Silent Key)
morse(sn, [.,.,.,-,.]).          % SN (understood, Sho' 'Nuff)


%return empty if empty input
helper3([],[],[]).
%All translated
helper3([], A, [R]):- morse(R,A).
%Found '^' (Boundary between letters)
    %If it's the first, move on to next
helper3(['^'| Tail], [], R):- helper3(Tail,[], R).
    %If it's not, it means we found the letter
    %convert it, save it, and move to the next letter
helper3(['^'| Tail], A, [RH | RT]):- morse(RH, A), helper3(Tail, [], RT).
%Found '#' (Boundary between words)
    %If it's the first, save it, and move on to next
helper3(['#'| Tail], [], ['#' | RT]):- helper3(Tail,[], RT).
    %If it's not, it means we the end of word
    %convert the letter using, save it and '#' , and move on
helper3(['#'| Tail], A, [RH, '#' | RT]):- morse(RH, A), helper3(Tail, [], RT).
%Otherwise, we're found '-' or '.'
%Append the head to the array, to be used later when '^' or '#' found
helper3([Head|Tail], A, R):- append(A,[Head], R2), helper3(Tail,R2,R).

signal_message([], []).
signal_message([Head | Tail], R1):- signal_morse([Head | Tail], R2), 
	helper3(R2,[],R1).
