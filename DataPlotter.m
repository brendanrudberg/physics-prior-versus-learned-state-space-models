data = readtable('C:\Users\rudbe\projects\IcelandData\temp_log_7_28_edited.csv');

% Convert x from Unix epoch seconds to datetime
x = datetime(data{:,5}, 'ConvertFrom', 'posixtime');

% Plot in desired legend order: Inside Air, Inside Wall, Outside Wall, Outside Air
figure
plot(x, data{:,2})  % Inside Air
hold on
plot(x, data{:,3})  % Inside Wall
plot(x, data{:,4})  % Outside Wall
plot(x, data{:,1})  % Outside Air
hold off

xlabel('Time')
ylabel('Temperature')
legend('Inside Air', 'Inside Wall', 'Outside Wall', 'Outside Air')
title('Temperature Data')