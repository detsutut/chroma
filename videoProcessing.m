[names,path] = uigetfile({'*.mp4;*.m4v;*.3gp;*.mov;*.wmv;*.avi;*.webm;*.mkv;*.flv;*.mpeg;*.mpg',...
    'Video files (*.mp4,*.avi,*.mkv,...)';'*.*',  'All Files (*.*)'},...
    'Frames Extractor - select video files','MultiSelect','on');
if iscell(names)==0
    name = names;
    tot = 1;
else
    tot = length(names);
end
for i = 1:tot
    if iscell(names)~=0
        name = names{i};
    end
    waittitle = strcat(name,' selected (',num2str(i),' out of',' ',num2str(tot),')');
    disp(waittitle);
    path_input = strcat(path,name);
    path_output = strcat(path,name,'.csv');
    mov = VideoReader(path_input);
    nof = floor(mov.duration*mov.FrameRate);
    palette = zeros(nof,3);
    k=0;
    waitmsg = strcat(num2str(k),'/',num2str(nof));
    f = waitbar(0,waitmsg,'Name',waittitle);
    for k = 3 : length(palette)
        if hasFrame(mov)==0
            break;
        end
        frame = readFrame(mov);
        rgb = mean(reshape(frame,[],size(frame,3)));
        palette(k,1)=rgb(1);
        palette(k,2)=rgb(2);
        palette(k,3)=rgb(3);
        waitmsg = strcat(num2str(k),'/',num2str(nof));
        waitbar(k/nof,f,waitmsg);
    end
    close(f)
    csvwrite(path_output,palette)
end