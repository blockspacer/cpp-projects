#pragma once

#include <QThread>
#include <QMutex>
#include <QWaitCondition>
#include <QSize>
#include <complex>

class render_thread : public QThread
{
    Q_OBJECT
public:
    render_thread(QMutex& load_mutex, size_t id = 0, size_t thread_count = 1, QObject* parent = nullptr);
    ~render_thread();
    void render(uchar* data, std::complex<double> center_offset, double scale, QSize image_size, size_t stride);
    void stop();
signals:
    void rendered();
protected:
    void run() override;

private:
    uchar value(const size_t& x, const size_t& y, const std::complex<double>&, const double&);

private:
    const size_t MAX_STEP = 100;

    double scale = 0.005;
    std::complex<double> center_offset = {0., 0.};
    QSize image_size;
    size_t block_size = 0;
    size_t width = 0;
    size_t height = 0;
    size_t stride = 0;

    uchar* data = nullptr;

    std::vector<uchar> local;


    size_t id = 0;
    size_t thread_count = 1;
    QMutex mutex;
    QMutex& load_mutex;
    QWaitCondition condition;
    std::atomic_bool restart = false;
    std::atomic_bool abort = false;
};

