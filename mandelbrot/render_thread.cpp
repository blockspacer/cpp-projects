#include "render_thread.h"
#include <QImage>
#include <QGuiApplication>
#include <iostream>
#include <algorithm>
#include <cstring>

render_thread::render_thread(QMutex& load_mutex, size_t id, size_t thread_count, QObject* parent) :
    QThread(parent), id(id), thread_count(thread_count), load_mutex(load_mutex) {
}

render_thread::~render_thread() {
    stop();
}

void render_thread::render(uchar* data, std::complex<double> center_offset, double scale, QSize image_size, size_t stride) {
    abort = false;
    QMutexLocker locker(&mutex);
    this->data = data;
    this->center_offset = center_offset;
    this->scale = scale;
    this->image_size = image_size;
    this->stride = stride;
    this->width = image_size.width();
    this->height = image_size.height();
    this->block_size = std::ceil(static_cast<double>(height) / thread_count);

    if (!isRunning()) {
       start(HighPriority);
    } else {
       restart = true;
       condition.wakeOne();
    }
}


void render_thread::stop() {
    {
        QMutexLocker locker(&mutex);
        abort = true;
        condition.wakeOne();
    }
    wait();
}

void render_thread::run() {
    forever {
        uchar * data = nullptr;
        std::complex<double> center_offset;
        double scale;
        size_t width, height, block_size,  stride;
        {
            QMutexLocker locker(&mutex);
            data = this->data;
            center_offset = this->center_offset;
            scale = this->scale;
            width = this->width;
            height = this->height;
            block_size = this->block_size;
            stride = this->stride;
        }

        size_t begin = id * block_size;
        size_t end = std::min(block_size, height - begin);

        size_t new_array_size = end * stride;
        local.resize(new_array_size);


        for (size_t step = width / 8; step >= 1; step /= 2) {
            for (size_t y = 0; y < end && !restart; y++) {
                uchar* p = local.data() + y * stride;
                for (size_t x = 0; x < width && !restart;) {
                    if (abort) {
                        return;
                    }

                    uchar val = value(x, y + begin, center_offset, scale);
                    for (size_t i = 0; i < step && x < width; i++, x++) {
                        *p++ = val;
                        *p++ = val;
                        *p++ = val;
                    }
                }
            }
            {
                QMutexLocker lg(&load_mutex);
                std::memcpy(data + stride * begin, local.data(), new_array_size);
                emit rendered();
            }
        }

        {
            QMutexLocker lg(&mutex);
            if (!restart)
                condition.wait(&mutex);
            restart = false;
        }
    }
}

uchar render_thread::value(const size_t& x, const size_t& y, const std::complex<double>& center_offset, const double& scale) {
    std::complex<double> c(x - width / 2., y - height / 2.);
    c *= scale;
    c += center_offset;

    std::complex<double> z = 0;
    size_t i = 0;
    while (std::abs(z) <= 2. && i < MAX_STEP) {
        z = z * z + c;
        ++i;
    }
    return static_cast<uint8_t>(255. - (static_cast<double>(i) / MAX_STEP) * 255.);
}



