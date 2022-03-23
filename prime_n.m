% This script prints out the first n prime numbers
% The script is optimised for the number of trials before identifying a 
% number as prime
% Script by Arman Hassanniakalager GitHub @hkalager
tic;
n=50;
prime_list=fun1(n);
disp(['The list of ',num2str(n),' first prime numbers is stored in variable',...
    '''prime_list''']) 
disp(prime_list);
toc;

function list_prime=fun1(n)
    n=round(n);
    if n<1
        error('Wrong choice of n')
    end
    list_prime=[];
    v=1;
    while(numel(list_prime)<n)
        v=v+1;
        vv=1;
        prev_divider=nan;
        cap_vv=v;
        while(isnan(prev_divider) && vv<cap_vv)
            vv=vv+1;
            if numel(list_prime)>0
                %highest_prime = list_prime(end);
                cap_vv=floor(v/2);
            end
            
            if mod(v,vv)==0
                prev_divider=true;
                vv=vv-1;
            end
        end
        if vv>=cap_vv || v==2
            list_prime(end+1)=v;
        end
    end
end
