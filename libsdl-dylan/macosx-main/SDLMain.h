/*   SDLMain.m - main entry point for our Cocoa-ized SDL app
       Initial Version: Darrell Walisser <dwaliss1@purdue.edu>
       Non-NIB-Code & other changes: Max Horn <max@quendi.de>

    Feel free to customize this file to suit your needs
    Customised for Gwydion Dylan by Rob Myers rob@robmyers.org
*/

#import <Cocoa/Cocoa.h>

// The Dylan main method callback
extern void (*SDL_Dylan_main)();

@interface SDLMain : NSObject
@end
