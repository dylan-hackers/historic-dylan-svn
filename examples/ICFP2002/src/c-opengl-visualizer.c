#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>

#if defined(__APPLE_CC__)
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

typedef unsigned int Boolean;
#define TRUE  1
#define FALSE 0

#define allocateArray(T, n) (T*)malloc(n * sizeof(T))

FILE *gSocket;
int gUnixSocket;

//------------------------------------------------------------------------

unsigned int gID;
unsigned int gMaxWeight;
unsigned int gMoney;

void readMyConfiguration(void)
{
    char buffer[4096];
    fgets(buffer, 4096, gSocket);
    sscanf(buffer, "%u %u %u", &gID, &gMaxWeight, &gMoney);
}

//------------------------------------------------------------------------

#define MAX_ROBOTS 2000

typedef struct
{
    Boolean playing;
    unsigned int x;
    unsigned int y;
}
Robot;

Robot gPlayers[MAX_ROBOTS];

void setOpenGLParametersForPlayer(unsigned int playerID)
{
    glColor3d(1.0, 0.0, 1.0);
}

//------------------------------------------------------------------------

unsigned int gMapWidth;
unsigned int gMapHeight;

typedef enum
{
    SPACE,
    BASE,
    WALL,
    WATER
}
TileType;

TileType **gMap;

TileType charToTileType(int character)
{
    switch(character)
    {
        case '.': return SPACE;
        case '@': return BASE;
        case '#': return WALL;
        case '~': return WATER;
    }

    printf("invalid map");
    exit(EXIT_FAILURE);
}

void readMap(void)
{
    char buffer[4096];
    fgets(buffer, 4096, gSocket);
    sscanf(buffer, "%u %u\n", &gMapWidth, &gMapHeight);

    gMap = allocateArray(TileType *, gMapHeight);
    unsigned int row;
    for (row = 0; row < gMapHeight; ++row)
    {
        gMap[row] = allocateArray(TileType, gMapWidth);

        fgets(buffer, 4096, gSocket);

        unsigned int col;
        for (col = 0; col < gMapWidth; ++col)
        {
            gMap[row][col] = charToTileType(buffer[col]);
        }
    }
}

void printTileType(TileType type)
{
    switch(type)
    {
        case SPACE: printf("."); return;
        case BASE:  printf("@"); return;
        case WALL:  printf("#"); return;
        case WATER: printf("~"); return;
    }

    printf("invalid map");
    exit(EXIT_FAILURE);
}

void printMap(void)
{
    unsigned int row;
    for (row = 0; row < gMapHeight; ++row)
    {
        unsigned int col;
        for (col = 0; col < gMapWidth; ++col)
        {
            printTileType(gMap[row][col]);
        }

        printf("\n");
    }
}

void setOpenGLParametersForTileType(TileType type)
{
    switch(type)
    {
        case SPACE: glColor3d(1.0, 1.0, 1.0); return;
        case BASE:  glColor3d(0.0, 1.0, 0.0); return;
        case WALL:  glColor3d(1.0, 0.0, 0.0); return;
        case WATER: glColor3d(0.0, 0.0, 1.0); return;
    }

    printf("invalid map");
    exit(EXIT_FAILURE);
}

//------------------------------------------------------------------------

char *parseUnsigned(unsigned int *number, char *string)
{
    unsigned int length = 0;
    while(string[length] >= '0' && string[length] <= '9')
    {
        ++length;
    }

    sscanf(string, "%u", number);

    return string + length;
}

char *parsePlayerAction(unsigned int playerID, char *string)
{
    switch (*string)
    {
        case 'N':
            ++gPlayers[playerID].y;
            break;
        case 'S':
            --gPlayers[playerID].y;
            break;
        case 'E':
            ++gPlayers[playerID].x;
            break;
        case 'W':
            --gPlayers[playerID].x;
            break;
            
        case 'P':
        case 'D':
        {
            unsigned int packageID;
            string += 2;
            string = parseUnsigned(&packageID, string);
            printf("Player %u picked up package id %u\n",
                   playerID,
                   packageID);
        }
        break;

        case 'X':
        {
            unsigned int x, y;
            string += 2;
            string = parseUnsigned(&x, string);
            string += 2;
            string = parseUnsigned(&y, string);
            printf("Player %u arrives at (%u, %u)\n",
                   playerID, x, y);

            gPlayers[playerID].x = x;
            gPlayers[playerID].y = y;
            gPlayers[playerID].playing = TRUE;
        }
        break;
    }

    return string;
}

char *parseServerResponse(char* string)
{
    ++string;

    unsigned int playerID;
    string = parseUnsigned(&playerID, string);
    printf("Player id is %u\n", playerID);

    ++string;

    while (*string != '#' && *string != '\n')
    {
        string = parsePlayerAction(playerID, string);

        if (*string == '\n')
        {
            break;
        }
        
        ++string;
    }

    return string;
}

void readServerResponse(void)
{
    char buffer[32768];
    fgets(buffer, 32768, gSocket);
    printf("Received server state update: %s", buffer);

    char* remainder = buffer;
    while(*remainder != '\n')
    {
        remainder = parseServerResponse(remainder);
    }
}

void readPackageList(void)
{
    char buffer[32768];
    fgets(buffer, 32768, gSocket);
    printf("Received package list: %s", buffer);
}

void sendAction(void)
{
    printf("sending \"100 Move N\\n\"\n");
    send(gUnixSocket, "100 Move N\n", 11, 0);
}

//------------------------------------------------------------------------

void setUpGameState(void)
{
    readMap();
    readMyConfiguration();
    readServerResponse();
}

//------------------------------------------------------------------------

void display(void)
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glBegin(GL_QUADS);

    unsigned int row;
    for (row = 0; row < gMapHeight; ++row)
    {
        unsigned int col;
        for (col = 0; col < gMapWidth; ++col)
        {
            setOpenGLParametersForTileType(gMap[row][col]);

            glVertex2d(col,     row);
            glVertex2d(col + 1, row);
            glVertex2d(col + 1, row + 1);
            glVertex2d(col,     row + 1);
        }
    }

    unsigned int playerID;
    for (playerID = 0; playerID < MAX_ROBOTS; ++playerID)
    {
        if (gPlayers[playerID].playing)
        {
            setOpenGLParametersForPlayer(playerID);
    
            glVertex2d(gPlayers[playerID].x + 0.25, gPlayers[playerID].y + 0.25);
            glVertex2d(gPlayers[playerID].x + 0.75, gPlayers[playerID].y + 0.25);
            glVertex2d(gPlayers[playerID].x + 0.75, gPlayers[playerID].y + 0.75);
            glVertex2d(gPlayers[playerID].x + 0.25, gPlayers[playerID].y + 0.75);
        }
    }
    
    glEnd();
    
    glutSwapBuffers();
}

void reshape(int w, int h)
{
    glViewport(0, 0, w, h);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluOrtho2D(0, gMapWidth, 0, gMapHeight);
    glMatrixMode(GL_MODELVIEW);

    glLoadIdentity();
}

void idle(void)
{
    readPackageList();
    sendAction();
    readServerResponse();
    
    glutPostRedisplay();
}

int main(int argc, char *argv[])
{
    glutInit(&argc, argv);
    glutInitWindowSize(512, 512);
    glutInitDisplayMode(GLUT_RGBA | GLUT_DEPTH | GLUT_DOUBLE);

    const char* serverHost = argv[1];
    unsigned short serverPort = atoi(argv[2]);

    struct sockaddr_in serverAddress;

    gUnixSocket = socket(AF_INET, SOCK_STREAM, 0);

    struct hostent *server
        = gethostbyname(serverHost);
    if (server == NULL)
    {
        printf("Can't get host %s by name\n", serverHost);
        exit(EXIT_FAILURE);
    }

#if !defined(linux)
    serverAddress.sin_len = 0;
#endif
    serverAddress.sin_family = AF_INET;
    serverAddress.sin_port = htons(serverPort);
    serverAddress.sin_addr.s_addr = *(in_addr_t*)(server->h_addr);
    memset(&(serverAddress.sin_zero), '\0', 8);
    
    int connectionResult = connect(gUnixSocket,
                                   (struct sockaddr *)&serverAddress,
                                   sizeof(struct sockaddr));
    if (connectionResult == -1)
    {
        printf("Can't connect to %s:%u\n", serverHost, serverPort);
        perror("Error");
        exit(EXIT_FAILURE);
    }

    send(gUnixSocket, "Player\n", 7, 0);

    gSocket = fdopen(gUnixSocket, "rb");
    if (gSocket == NULL)
    {
        printf("Can't convert socket to FILE*\n");
        exit(EXIT_FAILURE);
    }

    setUpGameState();

    printMap();

    printf("My ID: %u\n\
My Max Weight: %u\n\
My Money: %u\n", gID, gMaxWeight, gMoney);
    
    (void)glutCreateWindow("ICFP Contest 2002 Visual Client");
    glutDisplayFunc(display);
    glutReshapeFunc(reshape);
    glutIdleFunc(idle);

    glutMainLoop();

    return EXIT_SUCCESS;
}
