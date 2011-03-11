#include <string.h>
#include <tcl.h>
#include <tk.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include "camltk.h"

/*
 * Pixmap manipulation from OCaml : get the pixmap from an arbitrary photo
 * image, and put it back in some (possibly other) image.
 * TODO: other blits
 * We use the same format of "internal" pixmap data as in Tk, that is
 * 24 bits per pixel
 */

value camltk_getimgdata (value imgname) /* ML */
{
  Tk_PhotoHandle ph;
  Tk_PhotoImageBlock pib;
  int code,size;
  value res;

#if (TK_MAJOR_VERSION < 8)
  if (NULL == (ph = Tk_FindPhoto(String_val(imgname)))) 
    tk_error("no such image");
#else
  if (NULL == (ph = Tk_FindPhoto(cltclinterp, String_val(imgname)))) 
    tk_error("no such image");
#endif

  code = Tk_PhotoGetImage(ph,&pib); /* never fails ? */
  size = pib.width * pib.height * pib.pixelSize;
  res = alloc_string(size);

  /* no holes, default format ? */
  if ((pib.pixelSize == 3) &&
      (pib.pitch == (pib.width * pib.pixelSize)) &&
      (pib.offset[0] == 0) &&
      (pib.offset[1] == 1) &&
      (pib.offset[2] == 2)) {
    memcpy(pib.pixelPtr, String_val(res),size);
    return res;
  } else {
    int y;                      /* varies from 0 to height - 1 */
    int yoffs = 0;		/* byte offset of line in src */
    int yidx = 0;	        /* byte offset of line in dst */
    for (y=0; y<pib.height; y++,yoffs+=pib.pitch,yidx+=pib.width * 3) {
      int x;			/* varies from 0 to width - 1 */
      int xoffs = yoffs;	/* byte offset of pxl in src */
      int xidx = yidx;		/* byte offset of pxl in dst */
      for (x=0; x<pib.width; x++,xoffs+=pib.pixelSize,xidx+=3) {
	Byte(res, xidx) = pib.pixelPtr[xoffs+pib.offset[0]];
	Byte(res, xidx + 1) = pib.pixelPtr[xoffs+pib.offset[1]];
	Byte(res, xidx + 2) = pib.pixelPtr[xoffs+pib.offset[2]];
      };
    }
    return res;
  }
}

void
camltk_setimgdata_native (value imgname, value pixmap, value x, value y, 
		   value w, value h) /* ML */
{
  Tk_PhotoHandle ph;
  Tk_PhotoImageBlock pib;
  int code;

#if (TK_MAJOR_VERSION < 8)
  if (NULL == (ph = Tk_FindPhoto(String_val(imgname)))) 
    tk_error("no such image");
#else
  if (NULL == (ph = Tk_FindPhoto(cltclinterp, String_val(imgname)))) 
    tk_error("no such image");
#endif

  pib.pixelPtr = String_val(pixmap);
  pib.width = Int_val(w);
  pib.height = Int_val(h);
  pib.pitch = pib.width * 3;
  pib.pixelSize = 3;
  pib.offset[0] = 0;
  pib.offset[1] = 1;
  pib.offset[2] = 2;
  Tk_PhotoPutBlock(
#if (TK_MAJOR_VERSION == 8 && TK_MINOR_VERSION >= 5 || TK_MAJOR_VERSION > 8)
        NULL,
#endif
ph,&pib,Int_val(x),Int_val(y),Int_val(w),Int_val(h)
#if (TK_MAJOR_VERSION == 8 && TK_MINOR_VERSION >= 4 || TK_MAJOR_VERSION > 8)
                   , TK_PHOTO_COMPOSITE_SET
#endif
    );
}

void camltk_setimgdata_bytecode(argv,argn)
     value *argv;
     int argn;
{
  camltk_setimgdata_native(argv[0], argv[1], argv[2], argv[3],
			   argv[4], argv[5]);
}
