#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdbool.h>

#define unsafe_op(e) {fprintf(stdout, "Error with %s\n", e); exit(EXIT_FAILURE);}

#define SPLIT_BUFF 10
#define BUFFER 1024

void set_char_arr_vals(char *arr, char val, int len){
    for(int i=0; i<len; ++i) arr[i] = val;
}

char *read_file(const char *file_name, int *buf_sz){
    int fd;
    if((fd = open(file_name, O_RDONLY)) == -1) unsafe_op("open");

    struct stat sb;
    if(fstat(fd, &sb) == -1) unsafe_op("fstat");
    *buf_sz = (intmax_t)sb.st_size;

    char *buf; if((buf = malloc(*buf_sz)) == NULL) unsafe_op("malloc");
    read(fd, buf, *buf_sz);

    return buf;
}

char **get_lines(char *buf, int buf_sz, int *num_lines){
    int line_buf_sz = BUFFER;
    char **lines; if((lines = malloc(BUFFER * sizeof(char*))) == NULL) unsafe_op("malloc");
    *num_lines = 0;

    int cur_wrd_idx = 0;
    int cur_wrd_buf_sz = BUFFER;
    char *cur_wrd; if((cur_wrd = malloc(BUFFER * sizeof(char))) == NULL) unsafe_op("malloc");

    for(int i=0; i<buf_sz; ++i){
        if(buf[i] != '\n'){ // No new line
            cur_wrd[cur_wrd_idx++] = buf[i];
            if(cur_wrd_idx >= cur_wrd_buf_sz){ // prolly wont happen :P
                cur_wrd_buf_sz += BUFFER;
                if((cur_wrd = realloc(cur_wrd, cur_wrd_buf_sz * sizeof(char))) == NULL) unsafe_op("realloc");
            }
        } else { // new word
            cur_wrd[cur_wrd_idx] = '\0';
            lines[(*num_lines)++] = cur_wrd;

            if(*num_lines >= line_buf_sz){
                line_buf_sz += BUFFER;
                if((lines = realloc(lines, line_buf_sz * sizeof(char*))) == NULL) unsafe_op("realloc");
            }

            cur_wrd_idx = 0;
            if((cur_wrd = malloc(BUFFER * sizeof(char))) == NULL) unsafe_op("malloc");
        }
    }

    cur_wrd[cur_wrd_idx] = '\0';
    lines[(*num_lines)++] = cur_wrd;

    free(buf); 
    buf = NULL;

    return lines;
}

char **split(char *str, const char *split_str, int *num_splits){
    char **splits; if((splits = malloc(SPLIT_BUFF * sizeof(char*))) == NULL) unsafe_op("malloc");
    const int str_len = strlen(str);
    const int split_len = strlen(split_str);

    int splits_buf_sz = SPLIT_BUFF;
    *num_splits = 0;

    int cur_wrd_idx = 0;
    int cur_wrd_buf_sz = BUFFER;
    char *cur_wrd; if((cur_wrd = malloc(BUFFER * sizeof(char))) == NULL) unsafe_op("malloc");

    int cur_split_idx = 0;
    char split_buf[split_len];
    set_char_arr_vals(split_buf, '\0', split_len);

    for(int i=0; i<str_len; ++i){
        if(str[i] != split_str[cur_split_idx]){ // Not in split str
            cur_split_idx = 0;

            int split_buf_str_len = strlen(split_buf);
            if(split_buf_str_len > 0){ // If I was in a failed split and I haven't added my split buff yet
                
                for(int j=0; j<split_buf_str_len; ++j){
                    cur_wrd[cur_wrd_idx++] = split_buf[j];
                    if(cur_wrd_idx >= cur_wrd_buf_sz){ // prolly wont happen :P (realloc)
                        cur_wrd_buf_sz += BUFFER;
                        if((cur_wrd = realloc(cur_wrd, cur_wrd_buf_sz * sizeof(char))) == NULL) unsafe_op("realloc");
                    }
                }
            }

            cur_wrd[cur_wrd_idx++] = str[i];
            if(cur_wrd_idx >= cur_wrd_buf_sz){ // prolly wont happen :P
                cur_wrd_buf_sz += BUFFER;
                if((cur_wrd = realloc(cur_wrd, cur_wrd_buf_sz * sizeof(char))) == NULL) unsafe_op("realloc");
            }
        } else {
            split_buf[cur_split_idx] = split_str[cur_split_idx];
            cur_split_idx++;
            
            if((cur_split_idx) == split_len){
                set_char_arr_vals(split_buf, '\0', split_len); // We found a split so reset split_buf to 0
                
                cur_wrd[cur_wrd_idx] = '\0';
                if(strlen(cur_wrd) > 0) splits[(*num_splits)++] = cur_wrd;

                if(*num_splits >= splits_buf_sz){
                    splits_buf_sz += SPLIT_BUFF;
                    if((splits = realloc(splits, splits_buf_sz * sizeof(char*))) == NULL) unsafe_op("realloc");
                }

                cur_wrd_idx = 0;
                if((cur_wrd = malloc(BUFFER * sizeof(char))) == NULL) unsafe_op("malloc");

                cur_split_idx = 0;
            }
        }
    }   


    int split_buf_str_len = strlen(split_buf);
    if(split_buf_str_len > 0){ // If I was in a failed split and I haven't added my split buff yet
        
        for(int j=0; j<split_buf_str_len; ++j){
            cur_wrd[cur_wrd_idx++] = split_buf[j];
            if(cur_wrd_idx >= cur_wrd_buf_sz){ // prolly wont happen :P (realloc)
                cur_wrd_buf_sz += BUFFER;
                if((cur_wrd = realloc(cur_wrd, cur_wrd_buf_sz * sizeof(char))) == NULL) unsafe_op("realloc");
            }
        }
    } 

    cur_wrd[cur_wrd_idx] = '\0';
    if(strlen(cur_wrd) > 0) splits[(*num_splits)++] = cur_wrd;

    return splits;
}


int main(int argc, char **argv){

    char *file_name = "../intest.txt";
    if(argc >= 2) file_name = argv[1];

    int buf_sz;
    int num_lines;

    char *buf = read_file(file_name, &buf_sz);
    char **lines = get_lines(buf, buf_sz, &num_lines);

    for(int line_idx = 0; line_idx < num_lines; ++line_idx){
        char *line = lines[line_idx];

    }

    printf("Part One: \n");
    printf("Part Two: \n");

    return 0;
}
