#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
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
int gBid;

void readMyConfiguration(void)
{
    char buffer[4096];
    fgets(buffer, 4096, gSocket);
    sscanf(buffer, "%u %u %u", &gID, &gMaxWeight, &gMoney);

    gBid = 1;
}

int gMoveChar;

//------------------------------------------------------------------------

void setOpenGLParametersForPackage(void)
{
    glColor3d(0.0, 0.0, 0.0);
}

//------------------------------------------------------------------------

#define MAX_ROBOTS 2000

typedef struct
{
    Boolean playing;
    Boolean updatedThisFrame;
    unsigned int packageCount;
    unsigned int x;
    unsigned int y;
}
Robot;

Robot gPlayers[MAX_ROBOTS];

void setOpenGLParametersForPlayer(unsigned int playerID)
{
    if (playerID == gID)
    {
        glColor3d(0.0, 1.0, 1.0);
    }
    else
    {
        glColor3d(1.0, 0.0, 1.0);
    }
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

typedef struct
{
    TileType type;
    unsigned int possiblePackageCount;
}
Tile;

Tile **gMap;

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
    unsigned int row;
    
    char buffer[4096];
    fgets(buffer, 4096, gSocket);
    sscanf(buffer, "%u %u\n", &gMapWidth, &gMapHeight);

    gMap = allocateArray(Tile *, gMapHeight);
    
    for (row = 0; row < gMapHeight; ++row)
    {
        unsigned int col;

        gMap[row] = allocateArray(Tile, gMapWidth);

        fgets(buffer, 4096, gSocket);

        for (col = 0; col < gMapWidth; ++col)
        {
            gMap[row][col].type = charToTileType(buffer[col]);
            gMap[row][col].possiblePackageCount
                = (gMap[row][col].type == BASE ? ~0 : 0);
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
            printTileType(gMap[row][col].type);
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
        {
            unsigned int packageID;
            string += 2;
            string = parseUnsigned(&packageID, string);
            printf("Player %u picked up package id %u\n",
                   playerID,
                   packageID);
            ++gPlayers[playerID].packageCount;
            --gMap[gPlayers[playerID].y][gPlayers[playerID].x].possiblePackageCount;
        }
        break;
        case 'D':
        {
            unsigned int packageID;
            string += 2;
            string = parseUnsigned(&packageID, string);
            printf("Player %u dropped package id %u\n",
                   playerID,
                   packageID);
            if (gPlayers[playerID].packageCount > 0)
            {
                --gPlayers[playerID].packageCount;
            }
            ++gMap[gPlayers[playerID].y][gPlayers[playerID].x].possiblePackageCount;
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

            gPlayers[playerID].x = x - 1;
            gPlayers[playerID].y = y - 1;
            gPlayers[playerID].playing = TRUE;
            gPlayers[playerID].packageCount = 0;
        }
        break;
    }

    return string;
}

char *parseServerResponse(char* string)
{
    unsigned int playerID;
    if (sscanf(string, "Robot %u died.\n", &playerID) == 1)
    {
        if (playerID == gID)
        {
            exit(EXIT_FAILURE);
        }
        else
        {
            return "\n";
        }
    }

    ++string;

    
    string = parseUnsigned(&playerID, string);
    printf("Player id is %u\n", playerID);
    gPlayers[playerID].updatedThisFrame = TRUE;

    if (*string == '\n')
    {
        return string;
    }
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
    char *remainder;

    char buffer[32768];
    fgets(buffer, 32768, gSocket);
    printf("Received server state update: %s", buffer);

    remainder = buffer;
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
    char action[4096];
    
    if (gMoveChar != '\0')
    {
        sprintf(action, "%d Move %c\n", gBid, gMoveChar);
        gMoveChar = '\0';
    }
    else
    {
        sprintf(action, "%d Pick 1\n", gBid);
    }

    send(gUnixSocket, action, strlen(action), 0);
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
    unsigned int row;
    unsigned int playerID;
    
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glBegin(GL_QUADS);
    
    for (row = 0; row < gMapHeight; ++row)
    {
        unsigned int col;
        for (col = 0; col < gMapWidth; ++col)
        {
            setOpenGLParametersForTileType(gMap[row][col].type);

            glVertex2d(col,     row);
            glVertex2d(col + 1, row);
            glVertex2d(col + 1, row + 1);
            glVertex2d(col,     row + 1);

            if (gMap[row][col].possiblePackageCount > 0)
            {
                setOpenGLParametersForPackage();

                glVertex2d(col + 0.35, row + 0.35);
                glVertex2d(col + 0.65, row + 0.35);
                glVertex2d(col + 0.65, row + 0.65);
                glVertex2d(col + 0.35, row + 0.65);
            }
        }
    }

    for (playerID = 0; playerID < MAX_ROBOTS; ++playerID)
    {
        if (gPlayers[playerID].playing)
        {
            setOpenGLParametersForPlayer(playerID);
    
            glVertex2d(gPlayers[playerID].x + 0.25, gPlayers[playerID].y + 0.25);
            glVertex2d(gPlayers[playerID].x + 0.75, gPlayers[playerID].y + 0.25);
            glVertex2d(gPlayers[playerID].x + 0.75, gPlayers[playerID].y + 0.75);
            glVertex2d(gPlayers[playerID].x + 0.25, gPlayers[playerID].y + 0.75);

            if (gPlayers[playerID].packageCount > 0)
            {
                setOpenGLParametersForPackage();

                glVertex2d(gPlayers[playerID].x + 0.35, gPlayers[playerID].y + 0.35);
                glVertex2d(gPlayers[playerID].x + 0.65, gPlayers[playerID].y + 0.35);
                glVertex2d(gPlayers[playerID].x + 0.65, gPlayers[playerID].y + 0.65);
                glVertex2d(gPlayers[playerID].x + 0.35, gPlayers[playerID].y + 0.65);
            }
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
    unsigned int playerID;
    
    readPackageList();
    sendAction();
    readServerResponse();

    for (playerID = 0; playerID < MAX_ROBOTS; ++playerID)
    {
        if (!gPlayers[playerID].updatedThisFrame)
        {
            gPlayers[playerID].playing = FALSE;
        }
        gPlayers[playerID].updatedThisFrame = FALSE;
    }
    
    glutPostRedisplay();
}

void specialKey(int key, int mouseX, int mouseY)
{
    gMoveChar = '\0';

    switch (key)
    {
        case GLUT_KEY_UP:    gMoveChar = 'N'; break;
        case GLUT_KEY_DOWN:  gMoveChar = 'S'; break;
        case GLUT_KEY_LEFT:  gMoveChar = 'W'; break;
        case GLUT_KEY_RIGHT: gMoveChar = 'E'; break;
    }
}

void key(unsigned char key, int mouseX, int mouseY)
{
    switch (key)
    {
        case '1': gBid = (gBid < 0 ?   -1 :    1); break;
        case '2': gBid = (gBid < 0 ?   -2 :    2); break;
        case '3': gBid = (gBid < 0 ?   -4 :    4); break;
        case '4': gBid = (gBid < 0 ?   -8 :    8); break;
        case '5': gBid = (gBid < 0 ?  -16 :   16); break;
        case '6': gBid = (gBid < 0 ?  -32 :   32); break;
        case '7': gBid = (gBid < 0 ?  -64 :   64); break;
        case '8': gBid = (gBid < 0 ? -128 :  128); break;
        case '9': gBid = (gBid < 0 ? -256 :  256); break;
        case '0': gBid = (gBid < 0 ? -512 :  512); break;
        case '-': gBid *= -1; break;
    }

    printf("Now bidding %d per move\n", gBid);
}

int main(int argc, char *argv[])
{
    const char *serverHost;
    unsigned short serverPort;
    struct sockaddr_in serverAddress;
    struct hostent *server;
    int connectionResult;
    char windowName[4096];

    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_RGBA | GLUT_DEPTH | GLUT_DOUBLE);

    serverHost = argv[1];
    serverPort = atoi(argv[2]);

    gUnixSocket = socket(AF_INET, SOCK_STREAM, 0);

    server = gethostbyname(serverHost);
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
    serverAddress.sin_addr.s_addr = *(u_int32_t*)(server->h_addr);
    memset(&(serverAddress.sin_zero), '\0', 8);
    
    connectionResult = connect(gUnixSocket,
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
    
    glutInitWindowSize(gMapWidth * 16, gMapHeight * 16);

    printMap();

    printf("My ID: %u\n\
My Max Weight: %u\n\
My Money: %u\n", gID, gMaxWeight, gMoney);

    sprintf(windowName, "Bot ID %u on %s:%u", gID, serverHost, serverPort);
    
    (void)glutCreateWindow(windowName);
    glutDisplayFunc(display);
    glutReshapeFunc(reshape);
    glutIdleFunc(idle);
    glutSpecialFunc(specialKey);
    glutKeyboardFunc(key);

    glutMainLoop();

    return EXIT_SUCCESS;
}
